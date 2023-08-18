module Syntax.Program where

import Syntax.Expr

------------------------------------------------------------

data Decl = Decl Name Expr

data Program = Program [Decl]