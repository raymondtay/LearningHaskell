module Print3Broken where

{--
  Scope is where a variable referred to by name is valid.
  Another word used with the same meaning is `visibility`
  because if a variable ins't visible then its  not in scope.

  Local bindings are bindings local to particular expressions. The primary delineation here from global bindings is that
  local binding cannot be imported by other programs or modules.

  Global or top-level bindings in HAskell mean bindings visible
  to all code withina module and if made avaiable, can be imported
  by other modules or programs. Global bindings in the sens ethat avariable is unconditionally visible throughout an entire program do not exist in Haskell.

  --}
printSecond :: IO()
printSecond = do
  putStrLn greeting

-- variable in module scope
greeting = "Yarr"

main :: IO ()
main = do
  -- which `greeting` is seen here?
  -- the expression `putStrLn greeting` refers to 
  -- the same name in local scope whilst the 
  -- `printSecond` refers to the name in the global scope.
  putStrLn greeting
  printSecond
    where greeting = "Yarrr"

