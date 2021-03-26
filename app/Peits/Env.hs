{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Peits.Env where

import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Development.Shake as S
import Development.Shake.Classes (Binary, Hashable, NFData)
import GHC.Generics (Generic)

data ShellCommand = ShellCommand
  { -- | The command to run
    shCommand :: Text,
    -- | List of arguments to pass to the command
    shArgs :: [Text],
    -- | Standard input to pass to the process
    shStdin :: Maybe Text,
    -- | Optional list of strings not passed to the command, but affects caching
    shExtra :: Maybe [Text]
  }
  deriving stock (Show, Eq, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance S.RuleResult ShellCommand = Text

data Env = Env
  { runShellCommand :: ShellCommand -> S.Action Text
  }
