module Browser
    ( Location, location
    ) where


import Dict (Dict)
import Dict
import Signal as S
import Signal (Channel, channel)
import LocalChannel as LC

type alias Location =
    { path   : List String
    , params : Dict String String
    , hash   : Maybe String
    }

type LocationUpdate
    = PathUpdate (List String)
    | ParamUpdate (String, Maybe String)
    | HashUpdate (Maybe String)

location : Channel Location
location = channel {path=[], params=Dict.empty, hash=Nothing}

location' : Channel LocationUpdate
location' = channel (PathUpdate [])

locationSignal : S.Signal Location
locationSignal = S.foldp updateLocation {path=[], params=Dict.empty, hash=Nothing} (S.subscribe location')

updateLocation : LocationUpdate -> Location -> Location
updateLocation upd loc = case upd of
    PathUpdate newPath ->
        {loc | path <- newPath }
    ParamUpdate (key, newValue) ->
        {loc | params <- Dict.update key (\_ -> newValue) loc.params }
    HashUpdate newHash ->
        {loc | hash <- newHash }

channelOfSearchParam : LC.LocalChannel String
channelOfSearchParam = LC.create (\a -> ParamUpdate ("search", Just a)) location'
