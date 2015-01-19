{-# LANGUAGE OverloadedStrings #-}

module Haste.Angular.Providers.LocationProvider where

import Haste.Foreign

type LocationProvider = Unpacked

html5Mode :: LocationProvider -> Bool -> IO ()
html5Mode = ffi "(function(a,b){return a.html5Mode(b)})"
