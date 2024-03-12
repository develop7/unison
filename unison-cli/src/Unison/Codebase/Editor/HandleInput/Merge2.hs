{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,
  )
where

import Control.Lens (view, (%~))
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Writer.CPS (Writer)
import Control.Monad.Trans.Writer.CPS qualified as Writer
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Semialign (Semialign (..), alignWith)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import U.Codebase.Branch qualified as V2 (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as V2.Branch
import U.Codebase.Causal qualified as V2.Causal
import U.Codebase.Reference (Reference, Reference' (..), TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Referent qualified as V2 (Referent)
import U.Codebase.Sqlite.DbId (ProjectId)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.DeclCoherencyCheck (IncoherentDeclReason (..), checkDeclCoherency)
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.HandleInput.Update2
  ( addDefinitionsToUnisonFile,
    getExistingReferencesNamed,
    getNamespaceDependentsOf,
    makeParsingEnv,
    prettyParseTypecheck,
    typecheckedUnisonFileToBranchUpdates,
  )
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Merge.Database (MergeDatabase (..), makeMergeDatabase, referent2to1)
import Unison.Merge.Diff qualified as Merge
import Unison.Merge.DiffOp qualified as Merge
import Unison.Merge.Libdeps qualified as Merge
import Unison.Merge.PreconditionViolation qualified as Merge
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.TwoOrThreeWay (TwoOrThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Referent' qualified as Referent'
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.UnisonFile (UnisonFile)
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), bifoldMapDefns, bimapDefns, unzipDefns, zipDefnsWith)
import Unison.Util.Map qualified as Map (for_, insertLookup)
import Unison.Util.Nametree (Nametree (..), flattenNametree, traverseNametreeWithName, unflattenNametree, zipNametreesOfDefns)
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Util.Star2 (Star2)
import Unison.Util.Star2 qualified as Star2
import Witch (unsafeFrom)
import Prelude hiding (unzip, zip)

handleMerge :: ProjectBranchName -> Cli ()
handleMerge bobBranchName = do
  Cli.Env {codebase} <- ask

  -- Create a bunch of cached database lookup functions
  db <- makeMergeDatabase codebase

  -- Load the current project branch ("alice"), and the branch from the same project to merge in ("bob")
  mergeInfo <- getMergeInfo bobBranchName

  (mergedLibdeps, mergedDefns, droppedDefns, unisonFile) <-
    Cli.runTransactionWithRollback \abort -> do
      -- Load alice, bob, and LCA branches
      branches <- loadV2Branches =<< loadV2Causals abort db mergeInfo

      -- Load alice, bob, and LCA definitions + decl names
      (declNames, defns) <- loadDefns abort db mergeInfo branches

      -- Load and merge alice and bob libdeps (this could be done later)
      mergedLibdeps <- do
        libdeps <- loadLibdeps branches
        libdepsToBranch0 db (Merge.mergeLibdeps getTwoFreshNames libdeps)

      -- Diff the definitions
      diff <- performDiff abort db mergeInfo defns

      -- TODO is swapping constructors' names handled correctly here?
      -- TODO is exchanging constructor for function handled correctly here?
      -- TODO is exchanging function for constructor handled correctly here?
      let (conflicted, unconflicted) = identifyConflicts diff

      PartitionedContents {changes, fileContents} <- partitionFileContents defns unconflicted

      let (mergedDefns, droppedDefns) =
            unzipDefns
              ( zipDefnsWith
                  runNamespaceUpdate
                  runNamespaceUpdate
                  (bimapDefns BiMultimap.range BiMultimap.range defns.lca)
                  (applyNamespaceChanges changes)
              )

      case Map.null conflicted.terms && Map.null conflicted.types of
        False -> do
          honkingConflicted <- assertConflictsSatisfyPreconditions conflicted
          let conflictedFileContents =
                let clonk ::
                      Name ->
                      (Referent.Id, Referent.Id) ->
                      TwoWay (Defns (Relation Name TermReferenceId) (Relation Name TypeReferenceId))
                    clonk name (alice, bob) =
                      let f = \case
                            Referent'.Ref' ref -> termsToDefns (Relation.singleton name ref)
                            Referent'.Con' (ConstructorReference ref _) _ -> typesToDefns (Relation.singleton name ref)
                       in TwoWay {alice = f alice, bob = f bob}
                    honk :: Name -> (TypeReferenceId, TypeReferenceId) -> TwoWay (Relation Name TypeReferenceId)
                    honk name (alice, bob) =
                      TwoWay
                        { alice = Relation.singleton name alice,
                          bob = Relation.singleton name bob
                        }
                    termsToDefns terms = Defns {terms, types = Relation.empty}
                    typesToDefns types = Defns {terms = Relation.empty, types}
                 in bifoldMapDefns
                      (Map.foldMapWithKey clonk)
                      (fmap typesToDefns . Map.foldMapWithKey honk)
                      honkingConflicted
          unisonFile <- makeUnisonFile2 abort codebase fileContents conflictedFileContents declNames
          pure (mergedLibdeps, mergedDefns, droppedDefns, unisonFile)
        True -> do
          unisonFile <- makeUnisonFile abort codebase fileContents (declNames.alice <> declNames.bob)
          pure (mergedLibdeps, mergedDefns, droppedDefns, unisonFile)

  let mergedBranch =
        mergedDefns
          & bimapDefns (unflattenNametree . BiMultimap.fromRange) (unflattenNametree . BiMultimap.fromRange)
          & zipNametreesOfDefns Map.empty Map.empty
          & nametreeToBranch0
          & Branch.setChildBranch NameSegment.libSegment (Branch.one mergedLibdeps)
          & Branch.transform0 (Codebase.runTransaction codebase)

  let mergedNames = Branch.toNames mergedBranch
  let ppedNames = mergedNames <> defnsToNames droppedDefns
  let pped = PPED.makePPED (PPE.namer ppedNames) (PPE.suffixifyByName ppedNames)
  let prettyUf = Pretty.prettyUnisonFile pped unisonFile
  currentPath <- Cli.getCurrentPath
  parsingEnv <- makeParsingEnv currentPath mergedNames
  prettyParseTypecheck unisonFile pped parsingEnv >>= \case
    Left prettyError -> do
      promptUser mergeInfo (Pretty.prettyUnisonFile pped unisonFile) mergedBranch
    Right tuf -> do
      mergedBranchPlusTuf <-
        Cli.runTransactionWithRollback \abort -> do
          updates <- typecheckedUnisonFileToBranchUpdates abort undefined tuf
          pure (Branch.batchUpdates updates mergedBranch)
      Cli.stepAt
        (textualDescriptionOfMerge mergeInfo)
        ( Path.unabsolute mergeInfo.paths.alice,
          const mergedBranchPlusTuf
        )

type Dropped a = a

type NamespaceUpdate ref a =
  StateT (Map Name ref) (Writer (Dropped (Map Name ref))) a

runNamespaceUpdate :: Map Name ref -> NamespaceUpdate ref () -> (Map Name ref, Dropped (Map Name ref))
runNamespaceUpdate refs update =
  Writer.runWriter (State.execStateT update refs)

performAddsAndUpdates :: Map Name ref -> NamespaceUpdate ref ()
performAddsAndUpdates addsAndUpdates = do
  Map.for_ addsAndUpdates \name newRef -> do
    refs0 <- State.get
    let (maybeOldRef, !refs1) = Map.insertLookup name newRef refs0
    State.put refs1
    whenJust maybeOldRef \oldRef ->
      lift (Writer.tell (Map.singleton name oldRef))

performDeletes :: Set Name -> NamespaceUpdate ref ()
performDeletes deletions = do
  refs0 <- State.get
  let (refs1, deleted) = Map.partitionWithKey (\name _ -> not $ Set.member name deletions) refs0
  State.put refs1
  lift (Writer.tell deleted)

nametreeToBranch0 :: forall m. Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference)) -> Branch0 m
nametreeToBranch0 nametree =
  Branch.branch0
    (rel2star defns.terms)
    (rel2star defns.types)
    (Branch.one . nametreeToBranch0 <$> nametree.children)
    Map.empty
  where
    defns =
      bimapDefns (Relation.swap . Relation.fromMap) (Relation.swap . Relation.fromMap) nametree.value

    rel2star :: Relation ref name -> Star2 ref name metadata
    rel2star rel =
      Star2.Star2 {fact = Relation.dom rel, d1 = rel, d2 = Relation.empty}

makeUnisonFile ::
  (forall x. Output -> Transaction x) ->
  Codebase IO Symbol Ann ->
  Defns (Relation Name TermReferenceId) (Relation Name TypeReferenceId) ->
  Map Name [Name] ->
  Transaction (UnisonFile Symbol Ann)
makeUnisonFile abort codebase defns declMap = do
  let lookupCons k = case Map.lookup k declMap of
        Nothing -> Left (error ("failed to find: " <> show k <> " in the declMap"))
        Just x -> Right x
  unisonFile <- do
    addDefinitionsToUnisonFile
      abort
      codebase
      -- todo: fix output
      (const lookupCons)
      (defns.terms, defns.types)
      UnisonFile.emptyUnisonFile
  pure unisonFile

makeUnisonFile2 ::
  (forall x. Output -> Transaction x) ->
  Codebase IO Symbol Ann ->
  Defns (Relation Name TermReferenceId) (Relation Name TypeReferenceId) ->
  TwoWay (Defns (Relation Name TermReferenceId) (Relation Name TypeReferenceId)) ->
  TwoWay (Map Name [Name]) ->
  Transaction (UnisonFile Symbol Ann)
makeUnisonFile2 abort codebase unconflicted conflicted declMap = do
  unconflictedFile <- do
    addDefinitionsToUnisonFile
      abort
      codebase
      (\_ declName -> Right (lookupCons declName (declMap.alice <> declMap.bob)))
      (unconflicted.terms, unconflicted.types)
      UnisonFile.emptyUnisonFile
  aliceFile <- do
    addDefinitionsToUnisonFile
      abort
      codebase
      (\_ declName -> Right (lookupCons declName declMap.alice))
      (conflicted.alice.terms, conflicted.alice.types)
      UnisonFile.emptyUnisonFile
  bobFile <- do
    addDefinitionsToUnisonFile
      abort
      codebase
      (\_ declName -> Right (lookupCons declName declMap.bob))
      (conflicted.bob.terms, conflicted.bob.types)
      UnisonFile.emptyUnisonFile
  pure wundefined
  where
    lookupCons declName m =
      case Map.lookup declName m of
        Nothing -> error (reportBug "E077058" ("Expected decl name " <> show declName <> " in constructor name map"))
        Just x -> x

data MergeInfo = MergeInfo
  { paths :: !(TwoWay Path.Absolute),
    projectBranches :: !(TwoWay ProjectBranch),
    project :: !Project
  }
  deriving stock (Generic)

textualDescriptionOfMerge :: MergeInfo -> Text
textualDescriptionOfMerge mergeInfo =
  let bobBranchText = into @Text (ProjectAndBranch mergeInfo.project.name mergeInfo.projectBranches.bob.name)
   in "merge-" <> bobBranchText

getMergeInfo :: ProjectBranchName -> Cli MergeInfo
getMergeInfo bobBranchName = do
  (ProjectAndBranch project aliceProjectBranch, _path) <- Cli.expectCurrentProjectBranch
  bobProjectBranch <- Cli.expectProjectBranchByName project bobBranchName
  let alicePath = Cli.projectBranchPath (ProjectAndBranch project.projectId aliceProjectBranch.branchId)
  let bobPath = Cli.projectBranchPath (ProjectAndBranch project.projectId bobProjectBranch.branchId)
  pure
    MergeInfo
      { paths = TwoWay alicePath bobPath,
        projectBranches = TwoWay aliceProjectBranch bobProjectBranch,
        project
      }

loadV2Causals ::
  (forall a. Output -> Transaction a) ->
  MergeDatabase ->
  MergeInfo ->
  Transaction (TwoOrThreeWay (V2.CausalBranch Transaction))
loadV2Causals abort db info = do
  alice <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute info.paths.alice)
  bob <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute info.paths.bob)
  lca <-
    Operations.lca alice.causalHash bob.causalHash >>= \case
      Nothing -> pure Nothing
      Just lcaCausalHash -> do
        -- If LCA == bob, then we are at or ahead of bob, so the merge is done.
        when (lcaCausalHash == bob.causalHash) do
          abort $
            Output.MergeAlreadyUpToDate
              (Right (ProjectAndBranch info.project info.projectBranches.bob))
              (Right (ProjectAndBranch info.project info.projectBranches.alice))
        Just <$> db.loadCausal lcaCausalHash
  pure TwoOrThreeWay {lca, alice, bob}

loadV2Branches :: TwoOrThreeWay (V2.CausalBranch Transaction) -> Transaction (TwoOrThreeWay (V2.Branch Transaction))
loadV2Branches causals = do
  alice <- causals.alice.value
  bob <- causals.bob.value
  lca <- for causals.lca \causal -> causal.value
  pure TwoOrThreeWay {lca, alice, bob}

loadDefns ::
  (forall a. Output -> Transaction a) ->
  MergeDatabase ->
  MergeInfo ->
  (TwoOrThreeWay (V2.Branch Transaction)) ->
  Transaction (TwoWay (Map Name [Name]), ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)))
loadDefns abort0 db info branches = do
  lcaDefns <-
    case branches.lca of
      Nothing -> pure (Defns BiMultimap.empty BiMultimap.empty)
      Just lcaBranch -> loadLcaDefinitions abort (referent2to1 db) lcaBranch
  aliceDefns0 <- loadNamespaceInfo abort db branches.alice
  (aliceDeclNames, aliceDefns1) <-
    assertNamespaceSatisfiesPreconditions db abort info.projectBranches.alice.name branches.alice aliceDefns0
  bobDefns0 <- loadNamespaceInfo abort db branches.bob
  (bobDeclNames, bobDefns1) <-
    assertNamespaceSatisfiesPreconditions db abort info.projectBranches.bob.name branches.bob bobDefns0
  pure
    ( TwoWay {alice = aliceDeclNames, bob = bobDeclNames},
      ThreeWay {lca = lcaDefns, alice = aliceDefns1, bob = bobDefns1}
    )
  where
    abort :: Merge.PreconditionViolation -> Transaction void
    abort =
      abort0 . mergePreconditionViolationToOutput

loadLibdeps ::
  TwoOrThreeWay (V2.Branch Transaction) ->
  Transaction (ThreeWay (Map NameSegment (V2.CausalBranch Transaction)))
loadLibdeps branches = do
  lca <-
    case branches.lca of
      Nothing -> pure Map.empty
      Just lcaBranch -> load lcaBranch
  alice <- load branches.alice
  bob <- load branches.bob
  pure ThreeWay {lca, alice, bob}
  where
    load :: V2.Branch Transaction -> Transaction (Map NameSegment (V2.CausalBranch Transaction))
    load branch =
      case Map.lookup NameSegment.libSegment branch.children of
        Nothing -> pure Map.empty
        Just libdepsCausal -> do
          libdepsBranch <- libdepsCausal.value
          pure libdepsBranch.children

-- Diff definitions, and bail if there are any conflicted aliases, which we can't currently handle.
performDiff ::
  (forall a. Output -> Transaction a) ->
  MergeDatabase ->
  MergeInfo ->
  ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Transaction (Defns (Map Name (TwoDiffsOp Referent)) (Map Name (TwoDiffsOp TypeReference)))
performDiff abort db info defns = do
  diffs <- Merge.nameBasedNamespaceDiff db defns
  abortIfAnyConflictedAliases (abort . mergePreconditionViolationToOutput) info.projectBranches defns.lca diffs
  pure
    Defns
      { terms = partitionDiff (view #terms <$> diffs),
        types = partitionDiff (view #types <$> diffs)
      }

data UnconflictedPartitionedDefns = UnconflictedPartitionedDefns
  { aliceAdditions :: Defns (Map Name Referent) (Map Name TypeReference),
    bobAdditions :: Defns (Map Name Referent) (Map Name TypeReference),
    bothAdditions :: Defns (Map Name Referent) (Map Name TypeReference),
    aliceUpdates :: Defns (Map Name Referent) (Map Name TypeReference),
    bobUpdates :: Defns (Map Name Referent) (Map Name TypeReference),
    bothUpdates :: Defns (Map Name Referent) (Map Name TypeReference),
    aliceDeletions :: Defns (Map Name Referent) (Map Name TypeReference),
    bobDeletions :: Defns (Map Name Referent) (Map Name TypeReference),
    bothDeletions :: Defns (Map Name Referent) (Map Name TypeReference)
  }

data PartitionedContents = PartitionedContents
  { changes :: NamespaceChanges,
    fileContents :: Defns (Relation Name TermReferenceId) (Relation Name TypeReferenceId)
  }

partitionFileContents :: ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) -> UnconflictedPartitionedDefns -> Transaction PartitionedContents
partitionFileContents defns unconflictedDefns = do
  let UnconflictedPartitionedDefns
        { aliceUpdates,
          bobUpdates,
          bothUpdates,
          aliceAdditions,
          bobAdditions,
          bothAdditions,
          aliceDeletions,
          bobDeletions,
          bothDeletions
        } = unconflictedDefns

  let aliceNames = defnsToNames (bimapDefns BiMultimap.range BiMultimap.range defns.alice)
  let bobNames = defnsToNames (bimapDefns BiMultimap.range BiMultimap.range defns.bob)

  let alicesReferences = foldMap (defnRefs bobNames) [aliceUpdates, aliceDeletions]
  let bobsReferences = foldMap (defnRefs aliceNames) [bobUpdates, bobDeletions]

  fileContentsMap <- do
    d0 <- (\(a, b) -> Defns {terms = Relation.domain a, types = Relation.domain b}) <$> getNamespaceDependentsOf aliceNames bobsReferences
    d1 <- (\(a, b) -> Defns {terms = Relation.domain a, types = Relation.domain b}) <$> getNamespaceDependentsOf bobNames alicesReferences
    pure (d0 <> d1)

  let lcaAddsAndUpdates =
        let candidates = aliceUpdates <> bobUpdates <> bothUpdates <> aliceAdditions <> bobAdditions <> bothAdditions
         in bimapDefns (Map.\\ fileContentsMap.terms) (Map.\\ fileContentsMap.types) candidates
  let lcaDeletions = bimapDefns Map.keysSet Map.keysSet (aliceDeletions <> bobDeletions <> bothDeletions)

  pure
    PartitionedContents
      { changes =
          NamespaceChanges
            { hello = lcaAddsAndUpdates,
              goodbye = lcaDeletions
            },
        fileContents = bimapDefns Relation.fromMultimap Relation.fromMultimap fileContentsMap
      }
  where
    defnRefs :: Names -> Defns (Map Name Referent) (Map Name TypeReference) -> Set Reference
    defnRefs names =
      flip getExistingReferencesNamed names . defnNames

    defnNames :: Defns (Map Name Referent) (Map Name TypeReference) -> Defns (Set Name) (Set Name)
    defnNames =
      bimapDefns Map.keysSet Map.keysSet

data NamespaceChanges = NamespaceChanges
  { hello :: Defns (Map Name Referent) (Map Name TypeReference),
    goodbye :: Defns (Set Name) (Set Name)
  }

applyNamespaceChanges :: NamespaceChanges -> Defns (NamespaceUpdate Referent ()) (NamespaceUpdate TypeReference ())
applyNamespaceChanges changes =
  Defns
    { terms = go changes.hello.terms changes.goodbye.terms,
      types = go changes.hello.types changes.goodbye.types
    }
  where
    go :: (Ord ref) => Map Name ref -> Set Name -> NamespaceUpdate ref ()
    go addsAndUpdates deletes = do
      performAddsAndUpdates addsAndUpdates
      performDeletes deletes

unconflictedAdditions :: UnconflictedPartitionedDefns -> Defns (Map Name Referent) (Map Name TypeReference)
unconflictedAdditions UnconflictedPartitionedDefns {aliceAdditions, bobAdditions, bothAdditions} =
  aliceAdditions <> bobAdditions <> bothAdditions

unconflictedUpdates :: UnconflictedPartitionedDefns -> Defns (Map Name Referent) (Map Name TypeReference)
unconflictedUpdates UnconflictedPartitionedDefns {aliceUpdates, bobUpdates, bothUpdates} =
  aliceUpdates <> bobUpdates <> bothUpdates

unconflictedDeletions :: UnconflictedPartitionedDefns -> Defns (Map Name Referent) (Map Name TypeReference)
unconflictedDeletions UnconflictedPartitionedDefns {aliceDeletions, bobDeletions, bothDeletions} =
  aliceDeletions <> bobDeletions <> bothDeletions

data PartitionState v = PartitionState
  { aliceAdditions :: !(Map Name v),
    bobAdditions :: !(Map Name v),
    bothAdditions :: !(Map Name v),
    aliceUpdates :: !(Map Name v),
    bobUpdates :: !(Map Name v),
    bothUpdates :: !(Map Name v),
    aliceDeletions :: !(Map Name v),
    bobDeletions :: !(Map Name v),
    bothDeletions :: !(Map Name v),
    conflicts :: !(Map Name (v, v))
  }
  deriving stock (Generic)

emptyPartitionState :: PartitionState v
emptyPartitionState =
  PartitionState
    { aliceAdditions = Map.empty,
      bobAdditions = Map.empty,
      bothAdditions = Map.empty,
      aliceUpdates = Map.empty,
      bobUpdates = Map.empty,
      bothUpdates = Map.empty,
      aliceDeletions = Map.empty,
      bobDeletions = Map.empty,
      bothDeletions = Map.empty,
      conflicts = Map.empty
    }

-- Partition definitions into conflicted and unconflicted.
identifyConflicts ::
  Defns (Map Name (TwoDiffsOp Referent)) (Map Name (TwoDiffsOp TypeReference)) ->
  ( Defns (Map Name (Referent, Referent)) (Map Name (TypeReference, TypeReference)),
    UnconflictedPartitionedDefns
  )
identifyConflicts defns =
  let termSt = Map.foldlWithKey phi emptyPartitionState defns.terms
      typeSt = Map.foldlWithKey phi emptyPartitionState defns.types

      phi :: forall v. PartitionState v -> Name -> TwoDiffsOp v -> PartitionState v
      phi = \st k -> \case
        Conflict a b -> st & #conflicts %~ Map.insert k (a, b)
        Addition actor a ->
          let l = case actor of
                Alice -> #aliceAdditions
                Bob -> #bobAdditions
                Both -> #bothAdditions
           in st & l %~ Map.insert k a
        Update actor a ->
          let l = case actor of
                Alice -> #aliceUpdates
                Bob -> #bobUpdates
                Both -> #bothUpdates
           in st & l %~ Map.insert k a
        Deletion actor a ->
          let l = case actor of
                Alice -> #aliceDeletions
                Bob -> #bobDeletions
                Both -> #bothDeletions
           in st & l %~ Map.insert k a
   in ( Defns termSt.conflicts typeSt.conflicts,
        UnconflictedPartitionedDefns
          { aliceAdditions = Defns termSt.aliceAdditions typeSt.aliceAdditions,
            bobAdditions = Defns termSt.bobAdditions typeSt.bobAdditions,
            bothAdditions = Defns termSt.bothAdditions typeSt.bothAdditions,
            aliceUpdates = Defns termSt.aliceUpdates typeSt.aliceUpdates,
            bobUpdates = Defns termSt.bobUpdates typeSt.bobUpdates,
            bothUpdates = Defns termSt.bothUpdates typeSt.bothUpdates,
            aliceDeletions = Defns termSt.aliceDeletions typeSt.aliceDeletions,
            bobDeletions = Defns termSt.bobDeletions typeSt.bobDeletions,
            bothDeletions = Defns termSt.bothDeletions typeSt.bothDeletions
          }
      )

defnsToNames :: Defns (Map Name Referent) (Map Name TypeReference) -> Names
defnsToNames Defns {terms, types} =
  Names.Names
    { terms = Relation.fromMap terms,
      types = Relation.fromMap types
    }

defnsToRel ::
  Defns (Map Name Referent) (Map Name TypeReference) ->
  (Relation Name TermReferenceId, Relation Name TypeReferenceId)
defnsToRel Defns {terms, types} =
  let termRefIds = flip mapMaybe terms \case
        Referent.Ref r -> case r of
          ReferenceBuiltin _ -> Nothing -- todo - explode
          ReferenceDerived r -> Just r
        Referent.Con _ _ -> Nothing -- todo - get the decl
      typeRefIds =
        flip mapMaybe types \case
          ReferenceBuiltin _ -> Nothing -- todo - explode
          ReferenceDerived r -> Just r
   in (Relation.fromMap termRefIds, Relation.fromMap typeRefIds)

promptUser ::
  MergeInfo ->
  Pretty ColorText ->
  Branch0 IO ->
  Cli a
promptUser mergeInfo prettyUnisonFile newBranch = do
  Cli.Env {writeSource} <- ask
  let currentProjectId = mergeInfo.project.projectId
  let targetBranchName = mergeInfo.projectBranches.bob.name
  let selfBranchName = mergeInfo.projectBranches.alice.name
  -- Small race condition: since picking a branch name and creating the branch happen in different
  -- transactions, creating could fail.
  temporaryBranchName <- Cli.runTransaction (findTemporaryBranchName currentProjectId targetBranchName selfBranchName)
  _temporaryBranchId <-
    HandleInput.Branch.doCreateBranch'
      (Branch.one newBranch)
      Nothing
      mergeInfo.project
      temporaryBranchName
      (textualDescriptionOfMerge mergeInfo)
  scratchFilePath <-
    Cli.getLatestFile <&> \case
      Nothing -> "scratch.u"
      Just (file, _) -> file
  liftIO $ writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile)
  -- todo: respond with some message
  Cli.returnEarlyWithoutOutput

findTemporaryBranchName :: ProjectId -> ProjectBranchName -> ProjectBranchName -> Transaction ProjectBranchName
findTemporaryBranchName projectId other self = do
  Cli.findTemporaryBranchName projectId preferred
  where
    preferred :: ProjectBranchName
    preferred =
      unsafeFrom @Text $
        "merge-"
          <> into @Text other
          <> "-into-"
          <> into @Text self

-- Load namespace info into memory.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadNamespaceInfo ::
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  MergeDatabase ->
  V2.Branch Transaction ->
  Transaction (Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference)))
loadNamespaceInfo abort db branch = do
  defns <- loadNamespaceInfo0 (referent2to1 db) branch
  assertNamespaceHasNoConflictedNames defns & onLeft abort

-- | Load all "namespace definitions" of a branch, which are all terms and type declarations *except* those defined
-- in the "lib" namespace.
loadNamespaceInfo0 ::
  (Monad m) =>
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Nametree (Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference))))
loadNamespaceInfo0 referent2to1 branch = do
  terms <-
    branch.terms
      & Map.map Map.keysSet
      & traverse (Set.traverse referent2to1)
  let types = Map.map Map.keysSet branch.types
  children <-
    for (Map.delete NameSegment.libSegment branch.children) \childCausal -> do
      childBranch <- childCausal.value
      loadNamespaceInfo0_ referent2to1 childBranch
  pure Nametree {value = Defns {terms, types}, children}

loadNamespaceInfo0_ ::
  (Monad m) =>
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Nametree (Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference))))
loadNamespaceInfo0_ referent2to1 branch = do
  terms <-
    branch.terms
      & Map.map Map.keysSet
      & traverse (Set.traverse referent2to1)
  let types = Map.map Map.keysSet branch.types
  children <-
    for branch.children \childCausal -> do
      childBranch <- childCausal.value
      loadNamespaceInfo0_ referent2to1 childBranch
  pure Nametree {value = Defns {terms, types}, children}

-- | Assert that there are no unconflicted names in a namespace.
assertNamespaceHasNoConflictedNames ::
  Nametree (Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference))) ->
  Either Merge.PreconditionViolation (Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference)))
assertNamespaceHasNoConflictedNames =
  traverseNametreeWithName \names defns -> do
    terms <-
      defns.terms & Map.traverseWithKey \name ->
        assertUnconflicted (Merge.ConflictedTermName (Name.fromReverseSegments (name :| names)))
    types <-
      defns.types & Map.traverseWithKey \name ->
        assertUnconflicted (Merge.ConflictedTypeName (Name.fromReverseSegments (name :| names)))
    pure Defns {terms, types}
  where
    assertUnconflicted :: (Set ref -> Merge.PreconditionViolation) -> Set ref -> Either Merge.PreconditionViolation ref
    assertUnconflicted conflicted refs =
      case Set.asSingleton refs of
        Nothing -> Left (conflicted refs)
        Just ref -> Right ref

-- Convert a merge precondition violation to an output message.
mergePreconditionViolationToOutput :: Merge.PreconditionViolation -> Output.Output
mergePreconditionViolationToOutput = \case
  Merge.ConflictedAliases branch name1 name2 -> Output.MergeConflictedAliases branch name1 name2
  Merge.ConflictedTermName name refs -> Output.MergeConflictedTermName name refs
  Merge.ConflictedTypeName name refs -> Output.MergeConflictedTypeName name refs
  Merge.ConflictInvolvingBuiltin name -> Output.MergeConflictInvolvingBuiltin name
  Merge.ConstructorAlias branch name1 name2 -> Output.MergeConstructorAlias branch name1 name2
  Merge.DefnsInLib -> Output.MergeDefnsInLib
  Merge.MissingConstructorName name -> Output.MergeMissingConstructorName name
  Merge.NestedDeclAlias name -> Output.MergeNestedDeclAlias name
  Merge.NoConstructorNames name -> Output.MergeNoConstructorNames name
  Merge.StrayConstructor name -> Output.MergeStrayConstructor name

-- Assert that a namespace satisfies a few preconditions.
--
-- Fails if:
--   * The "lib" namespace contains any top-level terms or decls. (Only child namespaces are expected here).
--   * Any type declarations are "incoherent" (see `checkDeclCoherency`)
assertNamespaceSatisfiesPreconditions ::
  MergeDatabase ->
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  ProjectBranchName ->
  V2.Branch Transaction ->
  (Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference))) ->
  Transaction (Map Name [Name], Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
assertNamespaceSatisfiesPreconditions db abort branchName branch defns = do
  Map.lookup NameSegment.libSegment branch.children `whenJust` \libdepsCausal -> do
    libdepsBranch <- libdepsCausal.value
    when (not (Map.null libdepsBranch.terms) || not (Map.null libdepsBranch.types)) do
      abort Merge.DefnsInLib
  declNames <-
    checkDeclCoherency db.loadDeclNumConstructors defns
      & onLeftM (abort . incoherentDeclReasonToMergePreconditionViolation)
  pure
    ( declNames,
      Defns
        { terms = flattenNametree (view #terms) defns,
          types = flattenNametree (view #types) defns
        }
    )
  where
    incoherentDeclReasonToMergePreconditionViolation :: IncoherentDeclReason -> Merge.PreconditionViolation
    incoherentDeclReasonToMergePreconditionViolation = \case
      IncoherentDeclReason'ConstructorAlias firstName secondName ->
        Merge.ConstructorAlias branchName firstName secondName
      IncoherentDeclReason'MissingConstructorName name -> Merge.MissingConstructorName name
      IncoherentDeclReason'NestedDeclAlias name -> Merge.NestedDeclAlias name
      IncoherentDeclReason'NoConstructorNames name -> Merge.NoConstructorNames name
      IncoherentDeclReason'StrayConstructor name -> Merge.StrayConstructor name

assertConflictsSatisfyPreconditions ::
  Defns (Map Name (Referent, Referent)) (Map Name (TypeReference, TypeReference)) ->
  Transaction (Defns (Map Name (Referent.Id, Referent.Id)) (Map Name (TypeReferenceId, TypeReferenceId)))
assertConflictsSatisfyPreconditions conflicts =
  undefined

-- Like `loadNamespaceInfo`, but for loading the LCA, which has fewer preconditions.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadLcaDefinitions ::
  (Monad m) =>
  (forall void. Merge.PreconditionViolation -> m void) ->
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
loadLcaDefinitions abort referent2to1 branch = do
  defns0 <- loadNamespaceInfo0 referent2to1 branch
  defns1 <- assertNamespaceHasNoConflictedNames defns0 & onLeft abort
  pure
    Defns
      { terms = flattenNametree (view #terms) defns1,
        types = flattenNametree (view #types) defns1
      }

abortIfAnyConflictedAliases ::
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  TwoWay ProjectBranch ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  TwoWay (Defns (Map Name (Merge.DiffOp (Synhashed Referent))) (Map Name (Merge.DiffOp (Synhashed TypeReference)))) ->
  Transaction ()
abortIfAnyConflictedAliases abort projectBranchNames lcaDefns diffs = do
  whenJust (findConflictedAlias lcaDefns diffs.alice) \(name1, name2) ->
    abort (Merge.ConflictedAliases projectBranchNames.alice.name name1 name2)
  whenJust (findConflictedAlias lcaDefns diffs.bob) \(name1, name2) ->
    abort (Merge.ConflictedAliases projectBranchNames.bob.name name1 name2)

-- @findConflictedAlias namespace diff@, given an old namespace and a diff to a new namespace, will return the first
-- "conflicted alias" encountered (if any), where a "conflicted alias" is a pair of names that referred to the same
-- thing in the old namespace, but different things in the new one.
--
-- For example, if the old namespace was
--
--   foo = #foo
--   bar = #foo
--
-- and the new namespace is
--
--   foo = #baz
--   bar = #qux
--
-- then (foo, bar) is a conflicted alias.
--
-- This function currently doesn't return whether the conflicted alias is a decl or a term, but it certainly could.
findConflictedAlias ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (Map Name (Merge.DiffOp (Synhashed Referent))) (Map Name (Merge.DiffOp (Synhashed TypeReference))) ->
  Maybe (Name, Name)
findConflictedAlias defns diff =
  asum [go defns.terms diff.terms, go defns.types diff.types]
  where
    go :: forall ref. (Ord ref) => BiMultimap ref Name -> Map Name (Merge.DiffOp (Synhashed ref)) -> Maybe (Name, Name)
    go namespace diff =
      asum (map f (Map.toList diff))
      where
        f :: (Name, Merge.DiffOp (Synhashed ref)) -> Maybe (Name, Name)
        f (name, op) =
          case op of
            Merge.Added _ -> Nothing
            Merge.Deleted _ -> Nothing
            Merge.Updated _ hashed1 ->
              BiMultimap.lookupPreimage name namespace
                & Set.delete name
                & Set.toList
                & map (g hashed1)
                & asum
          where
            g :: Synhashed ref -> Name -> Maybe (Name, Name)
            g hashed1 alias =
              case Map.lookup alias diff of
                Just (Merge.Updated _ hashed2) | hashed1 == hashed2 -> Nothing
                _ -> Just (name, alias)

-- Given a name like "base", try "base__1", then "base__2", etc, until we find a name that doesn't
-- clash with any existing dependencies.
getTwoFreshNames :: Set NameSegment -> NameSegment -> (NameSegment, NameSegment)
getTwoFreshNames names name0 =
  go2 0
  where
    -- if
    --   name0 = "base"
    --   names = {"base__5", "base__6"}
    -- then
    --   go2 4 = ("base__4", "base__7")
    go2 :: Integer -> (NameSegment, NameSegment)
    go2 !i
      | Set.member name names = go2 (i + 1)
      | otherwise = (name, go1 (i + 1))
      where
        name = mangled i

    -- if
    --   name0 = "base"
    --   names = {"base__5", "base__6"}
    -- then
    --   go1 5 = "base__7"
    go1 :: Integer -> NameSegment
    go1 !i
      | Set.member name names = go1 (i + 1)
      | otherwise = name
      where
        name = mangled i

    mangled :: Integer -> NameSegment
    mangled i =
      NameSegment (NameSegment.toUnescapedText name0 <> "__" <> tShow i)

data TwoDiffsOp v
  = Conflict v v
  | Addition Actor v
  | Update Actor v -- new value
  | Deletion Actor v -- old value

data Actor
  = Alice
  | Bob
  | Both

------------------------------------------------------------------------------------------------------------------------
-- Conflicts

-- `getConflicts diffs` returns the set of conflicted names in `diffs`, where `diffs` contains two branches' diffs from
-- their LCA.
partitionDiff :: TwoWay (Map Name (Merge.DiffOp (Synhashed v))) -> Map Name (TwoDiffsOp v)
partitionDiff diffs =
  alignWith (f Alice Bob) diffs.alice diffs.bob
  where
    diffOpToTag :: Actor -> Merge.DiffOp (Synhashed v) -> TwoDiffsOp v
    diffOpToTag actor = \case
      Merge.Added x -> Addition actor x.value
      Merge.Updated _ x -> Update actor x.value
      Merge.Deleted x -> Deletion actor x.value

    f :: Actor -> Actor -> These (Merge.DiffOp (Synhashed v)) (Merge.DiffOp (Synhashed v)) -> TwoDiffsOp v
    f this that = \case
      These (Merge.Added x) (Merge.Added y) -> if x /= y then Conflict x.value y.value else Addition Both x.value
      These (Merge.Added _) (Merge.Updated _ _) -> error "impossible"
      These (Merge.Added _) (Merge.Deleted _) -> error "impossible"
      These (Merge.Updated _ x) (Merge.Updated _ y) -> if x /= y then Conflict x.value y.value else Update Both x.value
      -- Not a conflict, perhaps only temporarily, because it's easier to implement (we ignore these deletes):
      These (Merge.Updated _ x) (Merge.Deleted _) -> Update this x.value
      These (Merge.Updated _ _) (Merge.Added _) -> error "impossible"
      These (Merge.Deleted x) (Merge.Deleted _) -> Deletion Both x.value
      These a@(Merge.Deleted _) b -> f that this (These b a)
      This x -> diffOpToTag this x
      That x -> diffOpToTag that x

libdepsToBranch0 :: MergeDatabase -> Map NameSegment (V2.CausalBranch Transaction) -> Transaction (Branch0 Transaction)
libdepsToBranch0 db libdeps = do
  let branch :: V2.Branch Transaction
      branch =
        V2.Branch
          { terms = Map.empty,
            types = Map.empty,
            patches = Map.empty,
            children = libdeps
          }

  -- We make a fresh branch cache to load the branch of libdeps.
  -- It would probably be better to reuse the codebase's branch cache.
  -- FIXME how slow/bad is this without that branch cache?
  branchCache <- Sqlite.unsafeIO newBranchCache
  Conversions.branch2to1 branchCache db.loadDeclType branch
