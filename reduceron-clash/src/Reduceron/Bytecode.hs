module Reduceron.Bytecode where

import Clash.Prelude

type MaxFunArity = 7
type MaxConstructorArity = 6

type ToSpaceBits = 13
type HeapBits    = ToSpaceBits + 1
type StackBits   = 10
type ArityBits   = 1
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
type PrimInt     = Signed NumberBits
type ArgIndex    = Unsigned 8
type RegIndex    = Unsigned 8
type Update      = (UStackAddr, HeapAddr)



data Bytecode
  = {- 000 -} FUN  Arity FunAddr Bit
  | {- 001 -} PRIM Arity (BitVector 12)   -- TODO
  | {- 010 -} APP  AppPtr
  | {- 011 -} SAPP AppPtr
  | {- 100 -} INT  PrimInt
  | {- 101 -} CON  Arity ConIndex
  | {- 110 -} ARG  Bit ArgIndex
  | {- 111 -} REG  Bit RegIndex

