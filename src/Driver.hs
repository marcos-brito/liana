module Driver where

import qualified Data.Text as T
import Diagnostic
import Parser

data Session = Session
  { sessionFiles :: [FilePath],
    sessionDiags :: [Diagnostic],
    sessionConfig :: Config
  }

newtype Pass i o
  = Pass
  { runPass :: i -> Either [Diagnostic] o
  }

data Config = Config

newSession :: Session
newSession =
  Session
    { sessionFiles = [],
      sessionDiags = [],
      sessionConfig = defaultConfig
    }

defaultConfig :: Config
defaultConfig = Config

emit :: Session -> [Diagnostic] -> Session
emit sess diag = sess {sessionDiags = sessionDiags sess ++ diag}

compile :: Session -> FilePath -> T.Text -> Session
compile sess file src =
  case parse file src of
    Left diags -> emit sess diags
    Right ast -> error "todo"
