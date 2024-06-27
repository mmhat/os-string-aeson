-- | This library provides a well-typed representation of paths in a filesystem
-- directory tree.
--
-- Both "System.OsString.Aeson.Posix" and "System.OsString.Aeson.Windows"
-- provide the same interface. This module will reexport the appropriate module
-- for your platform.
module System.OsString.Aeson (module Export) where

#if defined(mingw32_HOST_OS)
import System.OsString.Aeson.Windows as Export
#else
import System.OsString.Aeson.Posix as Export
#endif
