import Lake
open Lake DSL

package «chess» where

lean_lib «Chess» where

lean_lib «Tests» where
  roots := #[`Test.Main]

lean_exe «chessDemo» {
  root := `Chess.Demo
}

@[test_driver]
lean_exe «test» {
  root := `Test.Main
}
