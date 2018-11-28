{-# LANGUAGE GADTs #-}

module Unison.Codebase.Editor where

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Foldable (toList)
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch, Branch0)
import Unison.Parser (Ann)
import Unison.Reference (Reference)
import Unison.Result (Result, Note)
import Unison.Names (Name, Referent)
import Unison.Util.Free (Free)
import qualified Unison.Parser as Parser
import qualified Unison.Result as Result
import qualified Unison.Typechecker.Context as Context
import qualified Unison.Util.Free as Free
import qualified Unison.UnisonFile as UF
import qualified Unison.PrettyPrintEnv as PPE

type BranchName = Name
type Source = Text -- "id x = x\nconst a b = a"
type SourceName = Text -- "foo.u" or "buffer 7"
type TypecheckingResult v = Result (Seq (Note v Ann))  (PPE.PrettyPrintEnv, Maybe (UF.TypecheckedUnisonFile' v Ann))

data AddOutput v
  = NothingToAdd
  | Added { -- Previously existed only in the file; now added to the codebase.
            successful :: UF.TypecheckedUnisonFile' v Ann
          -- Exists in the branch and the file, with the same name and contents.
          , duplicates :: UF.TypecheckedUnisonFile' v Ann
          -- Has a colliding name but a different definition than the codebase.
          , collisions :: UF.TypecheckedUnisonFile' v Ann }

data NameTarget = TermName | TypeName | PatternName

data Input
  = AliasI NameTarget Name Name
  | RenameI NameTarget Name Name
  | UnnameI NameTarget Name
  | AddI -- [Name]
  | ListBranchesI
  | SwitchBranchI BranchName
  | ForkBranchI BranchName
  | MergeBranchI BranchName
  | QuitI

data Notification
  = Success Input Bool

data Command v a where
  Input :: Command v (Either (TypecheckingResult v) Input)

  ReportParseErrors :: [Parser.Err v] -> Command v ()
  ReportTypeErrors :: PPE.PrettyPrintEnv -> [Context.ErrorNote v loc] -> Command v ()
  Notify :: Notification -> Command v ()

  Add :: BranchName -> UF.TypecheckedUnisonFile' v Ann -> Command v (AddOutput v)

  Typecheck :: SourceName -> Source -> Command v (TypecheckingResult v)

  -- Loads a branch by name from the codebase, returning `Nothing` if not found.
  LoadBranch :: BranchName -> Command v (Maybe Branch)

  -- Returns `Nothing` if a branch by that name already exists.
  NewBranch :: BranchName -> Command v (Maybe Branch)

  -- Create a new branch which is a copy of the given branch, and assign the
  -- forked branch the given name. Returns `False` if the forked branch name
  -- already exists.
  ForkBranch :: Branch -> BranchName -> Command v Bool

  -- Merges the branch with the existing branch with the given name. Returns
  -- `Nothing` if no branch with that name exists.
  MergeBranch :: BranchName -> Branch -> Command v (Maybe Branch)

  -- Return the subset of the branch tip which is in a conflicted state
  GetConflicts :: BranchName -> Command v (Maybe Branch0)

  -- Tell the UI to display a set of conflicts
  DisplayConflicts :: Branch0 -> Command v ()

  --
  -- LookupTerm :: BranchName -> Name -> Command v (Maybe Referent)
  -- LookupType :: BranchName -> Name -> Command v (Maybe Reference)
  -- LookupPattern :: BranchName -> Name -> Command v (Maybe (Reference, Int))
  AddTermName :: BranchName -> Referent -> Name -> Command v ()
  AddTypeName :: BranchName -> Reference -> Name -> Command v ()
  AddPatternName :: BranchName -> Reference -> Int -> Name -> Command v ()
  RemoveTermName :: BranchName -> Referent -> Name -> Command v ()
  RemoveTypeName :: BranchName -> Reference -> Name -> Command v ()
  RemovePatternName :: BranchName -> Reference -> Int -> Name -> Command v ()

  Alias :: BranchName -> NameTarget -> Name -> Name -> Command v Bool
  Rename :: BranchName -> NameTarget -> Name -> Name -> Command v Bool
  Unname :: BranchName -> NameTarget -> Name -> Command v Bool

  -- DisplayAliasFailure :: NameTarget -> Name -> Name


  -- AddName :: BranchName -> Name -> Name -> Command v Bool


  -- CurrentBranch :: Command v (Name, Branch)
  -- SwitchBranch :: Name -> Command (Maybe Branch)

  -- RemainingWork :: Branch -> Command v [RemainingWork]

  -- idea here is to find "close matches" of stuff in the input file, so
  -- can suggest use of preexisting definitions
  -- Search :: UF.TypecheckedUnisonFile' v Ann -> Command v (UF.TypecheckedUnisonFile' v Ann?)


commandLine :: Codebase IO v Ann -> Free (Command v) a -> IO a
commandLine _codebase command = do
  -- set up file watching...
  go command
  where
    go :: Free (Command v) a -> IO a
    go (Free.Pure a) = pure a
    go (Free.Bind cmd k) = case cmd of
      Alias _branchName _nameTarget _fromName _toName -> do
        success <- undefined -- Codebase.alias codebase branchName nameTarget fromName toName
        if success
          then putStrLn "Great job naming that alias! \129322"
          else putStrLn "Failed to something the something \129322"
        go (k success)

      _ -> error "todo"

type LoopState = BranchName

loop :: LoopState -> Free (Command v) ()
loop branchName = Free.unfold' go branchName where
  go :: LoopState -> Free (Command v) (Either () LoopState)
  go branchName = do
    e <- Free.eval Input
    case e of
      Left (Result.Result notes r) -> case r of
        Nothing -> do -- parsing failed
          Free.eval $
            ReportParseErrors [ err | Result.Parsing err <- toList notes]
          repeat
        Just (errorEnv, r) -> case r of
          Nothing -> do -- typechecking failed
            Free.eval $ ReportTypeErrors errorEnv
                          [ err | Result.TypeError err <- toList notes]
            repeat
          Just unisonFile -> undefined unisonFile
      Right input -> case input of
        AliasI nameTarget oldName newName -> do
          (Free.eval $ Alias branchName nameTarget oldName newName) >>=
            (Free.eval . Notify . Success input)
          repeat
        RenameI nameTarget oldName newName -> do
          (Free.eval $ Rename branchName nameTarget oldName newName) >>=
            (Free.eval . Notify . Success input)
          repeat
        UnnameI nameTarget name -> do
          (Free.eval $ Unname branchName nameTarget name) >>=
            (Free.eval . Notify . Success input)
          repeat
        AddI -> error "todo"
        ListBranchesI -> error "todo"
        SwitchBranchI _branchName -> pure $ Right branchName
        ForkBranchI _branchName -> do
          error "todo"
          -- repeat
        MergeBranchI _branchName -> error "todo"
        QuitI -> quit
    where
      repeat = pure $ Right branchName
      quit = pure $ Left ()

-- commandLine :: Codebase -> IO (Command v) a -> IO a
--
-- interact :: IO Line -> IO (SourceName, Source) -> FreeT (Command v) IO ()
--
-- -- data Free f a = Pure a | forall x . Bind (f x) (x -> Free f a)
--
-- do
--   line <- lift $ getLine
