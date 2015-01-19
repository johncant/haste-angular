{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
--{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Haste.Angular.Ng where

import Haste
import Haste.Foreign
import Control.Monad.State.Lazy
import Control.Monad.Reader.Class

type Ng = Unpacked
type Module = Unpacked
type Controller = Unpacked
type Scope = Unpacked

type MonadNg a = StateT Ng IO a
type MonadModule a = StateT Module IO a


type Inject = Unpacked


class (Unpack i, Unpack (Fn i)) => Injectable i where
  type Fn i
  injectList :: i -> [JSString]
  injectFun :: i -> Fn i
  inject :: i -> IO Inject
  inject inj = ffi "(function(l,f){console.log(arguments); l.push(f);return l})" (injectList inj) (injectFun inj)

instance (Unpack r) => Injectable (IO r) where
  type Fn (IO r) = IO r
  injectList _ = []
  injectFun = id
instance (Pack a, Unpack r) => Injectable (JSString, a -> IO r) where
  type Fn (JSString, a -> IO r) = a -> IO r
  injectList (a, fun) = [a]
  injectFun (_, fun) = fun
instance (Pack a, Pack b, Unpack r) => Injectable (JSString, JSString, a -> b -> IO r) where
  type Fn (JSString, JSString, a -> b -> IO r) = a -> b -> IO r
  injectList (a, b, fun) = [a,b]
  injectFun (_,_, fun) = fun
instance (Pack a, Pack b, Pack c, Unpack r) => Injectable (JSString, JSString, JSString, a -> b -> c -> IO r) where
  type Fn (JSString, JSString, JSString, a -> b -> c -> IO r) = a -> b -> c -> IO r
  injectList (a, b, c, fun) = [a,b,c]
  injectFun (_,_,_, fun) = fun
instance (Pack a, Pack b, Pack c, Pack d, Unpack r) => Injectable (JSString, JSString, JSString, JSString, a -> b -> c -> d -> IO r) where
  type Fn (JSString, JSString, JSString, JSString, a -> b -> c -> d -> IO r) = a -> b -> c -> d -> IO r
  injectList (a, b, c, d, fun) = [a,b,c,d]
  injectFun (_,_,_,_, fun) = fun



ng :: IO Ng
ng = (liftIO $ ffi "angular")

runAngular :: MonadNg () -> IO ()
runAngular defs = do
  ngI <- ng
  execStateT defs ngI
  return ()

module' :: (Injectable i) => JSString -> [JSString] -> Maybe i -> MonadModule () -> MonadNg ()
module' name deps injected defs = do
  ng <- get
  liftIO $ do
    mod <- moduleInj ng name deps injected
    execStateT defs mod
    return ()
  where moduleInj :: (Injectable i) => Ng -> JSString -> [JSString] -> Maybe i -> IO Module
        moduleInj ng name deps (Just inj) = (ffi "(function(ng,mn,deps,fn){return ng.module(mn,deps,fn())})" ng name deps (inject inj))
        moduleInj ng name deps Nothing    = (ffi "(function(ng,mn,deps){debugger; return ng.module(mn,deps)})" ng name deps)

config :: (Injectable i) => i -> MonadModule ()
config inj = do
  m <- get
  liftIO $ ffi "(function(m,fn){console.log('foo'); debugger;return m.config(fn())})" m (inject inj)

controller :: (Injectable i) => JSString -> i -> MonadModule ()
controller n inj = do
  m <- get
  liftIO $ ffi "(function(m,n,fn){return m.controller(n,fn())})" m n (inject inj)

