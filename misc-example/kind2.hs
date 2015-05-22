{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

import Prelude hiding (Bool(..))

-- DataKindsでTrueとFalseを型に昇格させ
data Bool = True | False

type family Not (a :: Bool) :: Bool

type instance Not True = False
type instance Not False = True

false :: Not True ~ False => a
false = undefined

true :: Not False ~ True => a
true = undefined

-- コンパイル時に失敗します。
-- invalid :: Not True ~ True => a
-- invalid = undefined
