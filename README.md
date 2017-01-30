# Power
Haskell Library for manage Power with DBUS

This module use Haskell DBUS to manage power.

## Interface

The module export two functions :

* can _Capability_ : return an IO Bool to know if capability is enable on this computer;
* goToState _Capability_ : change computer state to _capability_. Ex: `goToState Suspend`.

## Capabilities

The list of capabilities manage by library are:

* Suspend: to suspend computer to RAM;
* Hibernate: to suspend computer to disk;
* Hybrid: to suspend computer to RAM and to disk.
