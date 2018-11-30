-- To allow name clashes in record fields
{-# LANGUAGE DuplicateRecordFields #-}

module Vars where

data SVType = Real | Integer | String | Boolean
data SVTypeVal = RealVal Double | IntegerVal Int | StringVal String | BooleanVal Bool
data SVCausality = Input | Output
data Port = Port {vRef :: Int,
                  causality :: SVCausality,
                  type' :: SVType,
                  val :: SVTypeVal}
