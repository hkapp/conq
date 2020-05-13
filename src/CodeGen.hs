module CodeGen where

import PrettyPrint

type CodeGen a = a -> Code
type Code = String
type Label = String

if_ :: Code -> Code -> Code -> Code
if_ cond thenBranch elseBranch = unlines [
  "if (" ++ cond ++ ") {",
  indent thenBranch,
  "}",
  "else {",
  indent elseBranch,
  "}"
  ]

goto :: Label -> Code
goto targetLabel = "goto " ++ targetLabel ++ ";"

label :: Label -> Code
label labelName = labelName ++ ":"

-- Unused / Deprecated

c_fcall :: Code -> [Code] -> Code
c_fcall fName fArgs = fName ++ "(" ++ (commaSeparated fArgs) ++ ")"

c_strncmp :: Code -> Code -> Int -> Code
c_strncmp testedString expectedString strLen = c_fcall "strncmp" [testedString, expectedString, (show strLen)]

c_global_success :: Code
c_global_success = "goto regex_success;"

c_global_failure :: Code
c_global_failure = "goto regex_failure;"
