module Power (
goToState, can) where

import DBus
import DBus.Client


data Capabilities = Suspend | Hibernate | Hybrid

-- DBUS methods :
-- org.freedesktop.PowerManagement
-- /org/freedesktop/PowerManagement
-- method bool org.freedesktop.PowerManagement.CanHibernate()
-- signal void org.freedesktop.PowerManagement.CanHibernateChanged(bool can_hibernate)
-- method bool org.freedesktop.PowerManagement.CanHybridSuspend()
-- signal void org.freedesktop.PowerManagement.CanHybridSuspendChanged(bool can_hybrid_suspend)
-- method bool org.freedesktop.PowerManagement.CanSuspend()
-- signal void org.freedesktop.PowerManagement.CanSuspendChanged(bool can_suspend)
-- method bool org.freedesktop.PowerManagement.GetPowerSaveStatus()
-- method void org.freedesktop.PowerManagement.Hibernate()
-- signal void org.freedesktop.PowerManagement.PowerSaveStatusChanged(bool save_power)
-- method void org.freedesktop.PowerManagement.Suspend()
-- method bool org.freedesktop.PowerManagement.Inhibit.HasInhibit()
-- signal void org.freedesktop.PowerManagement.Inhibit.HasInhibitChanged(bool has_inhibit)
-- method uint org.freedesktop.PowerManagement.Inhibit.Inhibit(QString application, QString reason)
-- method void org.freedesktop.PowerManagement.Inhibit.UnInhibit(uint cookie)
-- method QDBusVariant org.freedesktop.DBus.Properties.Get(QString interface_name, QString property_name)
-- method QVariantMap org.freedesktop.DBus.Properties.GetAll(QString interface_name)
-- method void org.freedesktop.DBus.Properties.Set(QString interface_name, QString property_name, QDBusVariant value)
-- method QString org.freedesktop.DBus.Introspectable.Introspect()
-- method QString org.freedesktop.DBus.Peer.GetMachineId()
-- method void org.freedesktop.DBus.Peer.Ping()

-- For my first usage, I just need Suspend and I know I have.
-- To be updated later.

can :: Capabilities -> IO Bool
can Suspend = dbusCallWithRetBool memberNameCanSuspend
can Hybrid  = dbusCallWithRetBool memberNameCanHybrid
can Hibernate = dbusCallWithRetBool memberNameCanHibernate


goToState :: Capabilities -> IO()
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

