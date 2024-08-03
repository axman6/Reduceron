{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Reduceron.Bytecode where


import Clash.Prelude
import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Data.Proxy (Proxy)

type MaxFunArity = 7
type MaxConstructorArity = 6

type ToSpaceBits = 13
type HeapBits    = ToSpaceBits + 1
type StackBits   = 10
type ArityBits   = 3
type FunBits     = 10
type UStackBits  = 9
type LStackBits  = 9
-- type UpdateSize  = UStackBits + HeapBits
type NumberBits  = HeapBits + 1

type Arity       = Unsigned ArityBits
type FunAddr     = Unsigned FunBits
type HeapAddr    = Unsigned HeapBits
type ToSpaceAddr = Unsigned ToSpaceBits
type UStackAddr  = Unsigned UStackBits
type LStackAddr  = Unsigned LStackBits

type AppPtr      = Unsigned 15
type ConIndex    = Unsigned 10
type PrimInt     = Signed   NumberBits
type ArgIndex    = Unsigned 8
type RegIndex    = Unsigned 8
type Update      = (UStackAddr, HeapAddr)


-- TODO: This should cover a lot more
data Prim
  = PPlus
  | PSub
  | PEQ
  | PNEQ
  | PLEQ
  | PAND
  | PST32
  | PLD32
  deriving (Show, Generic, ShowX, NFDataX)

-- TODO: This seems wasteful, and greatly limits the number of primitives - I want popcount!
deriveAnnotation (simpleDerivator OneHot OverlapL) [t|Prim|]
deriveBitPack [t|Prim|]

data Atom
  = {- 000 -} Fun  Arity FunAddr Bit
  | {- 001 -} Prim Arity Prim   -- TODO
  | {- 010 -} App  AppPtr
  | {- 011 -} SApp AppPtr
  | {- 100 -} INT  PrimInt
  | {- 101 -} CON  Arity ConIndex
  | {- 110 -} ARG  Bit ArgIndex
  | {- 111 -} REG  Bit RegIndex
  deriving (Show, Generic, ShowX, NFDataX)


deriveDefaultAnnotation [t|Atom|]
deriveBitPack [t|Atom|]

type AppArity = Unsigned 2
data Application = Application
  { arity     :: AppArity
  , nf        :: Bit
  , hasAlts   :: Bit
  , collected :: Bit
  , atoms     :: Vec 4 Atom
  } deriving (Show, Generic, ShowX, NFDataX)

deriving instance BitPack Application

mapApplicationAtoms :: (Atom -> Atom) -> Application -> Application
mapApplicationAtoms f Application{..} =
  Application { atoms = fmap f atoms, ..}

data Template = Template
  { offset     :: Signed 4
  , top        :: Atom
  , pushAlts   :: Bit
  , alts       :: Unsigned 10
  , instAtoms2 :: Bit
  , app2Header :: Unsigned 5
  , pushMask   :: BitVector 5
  , app2Atoms  :: Vec 5 Atom
  , instApp1   :: Bit
  , app1       :: Application
  , app1Prim   :: Bit
  , app2Prim   :: Bit
  , destReg1   :: Unsigned 4
  , destReg2   :: Unsigned 4
  } deriving (Show, Generic, ShowX, NFDataX)

deriving instance BitPack Template

mapTemplateAtoms :: (Atom -> Atom) -> Template -> Template
mapTemplateAtoms f Template {..} = Template
    { top = f top
    , app2Atoms = fmap f app2Atoms
    , app1 = mapApplicationAtoms f app1
    , ..}

mainAtom, falseAtom, trueAtom :: Atom
mainAtom  = Fun 1 0 0
falseAtom = CON 1 0
trueAtom  = CON 1 1

bool :: Bit -> Atom
bool b = CON 1 (fromIntegral b)