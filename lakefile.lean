import Lake
open Lake DSL
open System

package «finance» where
  -- add package configuration options here

lean_lib «Finance» where

lean_exe «demo» {
  root := `Demo.Main
}

@[test_driver]
lean_exe «test» {
  root := `Test.Basic
}

require mathlib from git
  "https://github.com/leanprover-community/mathlib4" @ "master"
