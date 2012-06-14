{-# LANGUAGE TemplateHaskell #-}

module Application where

import Snap
import Snap.Snaplet.Heist

data App = App {
  _heist :: Snaplet (Heist App)
  }
makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

type AppHandler = Handler App App
