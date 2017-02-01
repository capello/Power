-- | The Power Module is a module to change computer state to some suspend types.
-- You can hibernate, suspend or hybrid suspend.
--
-- The module use DBUS to perform actions, so you need au operating system with DBUS.
--
-- The interface of the module is very easy: one function to kwonw if a suspend system is available and an other to change the state.
--
module Power (
goToState, can, Capability) where

import DBus
import DBus.Client

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

-- DBUS methods :
-- method bool org.freedesktop.PowerManagement.CanHibernate()
-- method bool org.freedesktop.PowerManagement.CanHybridSuspend()
-- method bool org.freedesktop.PowerManagement.CanSuspend()
-- method void org.freedesktop.PowerManagement.Hibernate()
-- method void org.freedesktop.PowerManagement.Suspend()


-- | can function test if the capability is available on this computer.
-- take a Capability to check. return True if Capability is available on the computer, and False if it is not.s
can :: Capability -> IO Bool
can Suspend = dbusCallWithRetBool memberNameCanSuspend
can Hybrid  = return False -- dbusCallWithRetBool memberNameCanHybrid
can Hibernate = dbusCallWithRetBool memberNameCanHibernate

-- | goToState function change the state of the computer to the state required by the capability
-- passed as parameter. If capability is not available, do nothing.
goToState :: Capability -> IO()
goToState Suspend = do
  s <- can Suspend
  if s then dbusCall memberNameSuspend else return ()
goToState Hybrid  = do
  h <- can Hybrid
  if h then dbusCall memberNameHybrid else return ()
goToState Hibernate = do
  h <- can Hibernate
  if h then dbusCall memberNameHibernate else return ()

-- Call DBUS
dbusCallWithRetBool :: MemberName -> IO Bool
dbusCallWithRetBool m = do
  client <- connectSession
  reply <- call_ client (methodCall
                         objectPathPower
                         interfaceNamePower
                         m )
           { methodCallDestination= Just busNamePower }
  let Just isPossible = fromVariant (methodReturnBody reply !! 0)
  return (isPossible)

dbusCall :: MemberName -> IO ()
dbusCall m = do
  client <- connectSession
  callNoReply client (methodCall objectPathPower
                     interfaceNamePower
                     m)
    { methodCallDestination = Just busNamePower }
  return ()

-- Data constants (object path, interface name and all members)
objectPathPower = objectPath_ "/org/freedesktop/PowerManagement"
interfaceNamePower = interfaceName_ "org.freedesktop.PowerManagement"
busNamePower = busName_ "org.freedesktop.PowerManagement"
memberNameSuspend = memberName_ "Suspend"
memberNameCanSuspend = memberName_ "CanSuspend"
memberNameHibernate = memberName_ "Hibernate"
memberNameCanHibernate = memberName_ "CanHibernate"
memberNameHybrid = memberName_ "HybridSuspend"
memberNameCanHybrid = memberName_ "CanHybridSuspend"

