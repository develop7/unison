-- | @project.create-branch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectCreateBranch
  ( projectCreateBranch,
  )
where

import qualified Data.UUID.V4 as UUID
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (stepAt)
import Unison.Cli.ProjectUtils (getCurrentProjectBranch, projectBranchPath)
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectBranchName)
import Witch (into)

-- | Create a new branch in a project:
--
-- * If the user isn't on a branch in a project, bail.
-- * Otherwise, if a branch with the given name already exists in the current project, bail.
-- * Otherwise, create the (empty) branch and cd to it.
--
-- The race condition detailed in the comment on `projectCreate` affects this procedure, too. As there, we elect to do
-- nothing about it.
projectCreateBranch :: ProjectBranchName -> Cli ()
projectCreateBranch name = do
  (projectId, currentBranchId) <- getCurrentProjectBranch & onNothingM (error "not on branch")
  newBranchId <- liftIO (Queries.BranchId <$> UUID.nextRandom)

  Cli.runEitherTransaction do
    Queries.projectBranchExistsByName projectId (into @Text name) >>= \case
      False -> do
        Queries.insertBranch projectId newBranchId (into @Text name)
        Queries.markProjectBranchChild projectId currentBranchId newBranchId
        pure (Right ())
      True -> pure (Left (error "branch by that name exists"))

  let path = projectBranchPath projectId newBranchId
  Cli.stepAt "project.create-branch" (Path.unabsolute path, id) -- id creates empty branch
  Cli.cd path
