# haste-angular

Haskell bindings for AngularJS

## Project status

Experimental. I made bindings for module definition, config, controller
definition, and injection, but decided to give React a go before going
any further.

## Example

```haskell
import qualified Haste.Angular.Ng as Ng
import Haste.Prim

setupAppRoutes :: (JSString, JSString, JSString, UI.StateProvider -> UI.UrlRouterProvider -> LocationProvider -> IO ())
setupAppRoutes = ("$stateProvider", "$urlRouterProvider", "$locationProvider", \stateProvider urlRouterProvider locationProvider -> do

  -- bla

  return ()
  )

main = do
  Ng.runAngular $ do
    Ng.module' "photogrammetryApp" ["ui.router", "angularFileUpload"]
(Nothing :: Maybe (IO ())) $ do
      Ng.config setupAppRoutes
      Ng.controller "ImageUploaderController" -- bla, as long as it is injectable

  return ()
```

## Contributing

Pull requests are welcome
