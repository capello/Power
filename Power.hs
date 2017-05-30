-- | The Power Module is a module to change computer state to some suspend types.
-- You can hibernate, suspend or hybrid suspend.
--
-- The module use DBUS to perform actions, so you need au operating system with DBUS.
--
-- The interface of the module is very easy: one function to kwonw if a suspend system is available and an other to change the state.
--
module Power (
goToState, can, Capability) where

import System.Posix.Files as F
import Control.Applicative

-- | Capability enumerate the possibles Capapilities of computer states
--
--       [@Suspend@] suspend computer to RAM
--
--       [@Hibernate@] suspend computer to hard drive
--
--       [@Hybrid@] suspend computer also to RAM and to hard drive (Not Yet implemented)
data Capability = Suspend
                | Hibernate
                | Hybrid

stateFile = "/sys/power/state"

-- | can function test if the capability is available on this computer.
-- take a Capability to check. return True if Capability is available on the computer, and False if it is not.s
can :: Capability -> IO Bool
can Suspend = (&&) <$> (F.fileExist stateFile) <*> (fmap (any (=="mem")) readStateFile)
can Hybrid  = return False -- dbusCallWithRetBool memberNameCanHybrid
can Hibernate = (&&) <$> (F.fileExist stateFile) <*> (fmap (any (=="disk")) readStateFile)

-- | goToState function change the state of the computer to the state required by the capability
-- passed as parameter. If capability is not available, do nothing.
goToState :: Capability -> IO()
goToState Suspend = writeFile stateFile "mem"
goToState Hybrid  = writeFile stateFile "mem disk"
goToState Hibernate = writeFile stateFile "disk"

-- Read/Write stateFile
readStateFile :: IO [String]
readStateFile = fmap words $ readFile stateFile

