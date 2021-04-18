module Component.Test exposing
    ( DebugComponent, Program
    , element
    , application, applicationNoUrl
    )

{-| If you want to test your components you can use the automatic tests or you create a private
main method in your component and open the page in `elm reactor`.

These methods helps you to create small component tests that you can test with `elm reactor` without
writing a large boilerplate.

# Helper Types
@docs DebugComponent, Program

# Program
@docs element, document, application, applicationNoUrl

-}

import Platform
import Browser
import Browser.Navigation exposing (Key)
import Component
import Triple exposing (Triple)
import Html exposing (Html)
import Url exposing (Url)

-- HELPER TYPES

{-| The internal model alias that is used by `element`, `document`, `application` and
`applicationNoUrl`.
-}
type alias DebugComponent model event =
    { model: model
    , events: List event
    , key: Maybe Key
    }

{-| The test program type for component tests. This is an alias for `Platform.Program` but with an
internal model and no flags.
-}
type alias Program model msg event = Platform.Program () (DebugComponent model event) msg

viewEvents : Maybe (List event -> Html Never)
    -> List event
    -> Html msg
    -> Html msg
viewEvents viewer events component =
    case viewer of
        Just viewEv ->
            Html.div []
                [ component
                , Html.map never <| viewEv events
                ]
        Nothing ->
            component

update : (msg -> model -> Triple model (Cmd msg) (List event))
    -> Bool
    -> msg
    -> DebugComponent model event
    -> (DebugComponent model event, Cmd msg)
update updateComp keepEvents msg comp =
    let
        result : Triple model (Cmd msg) (List event)
        result = updateComp msg comp.model
    in 
        Tuple.pair
            { comp
            | model = Triple.first result
            , events =
                if keepEvents
                then Triple.third result ++ comp.events
                else comp.events
            }
        <| Triple.second result

-- PROGRAM

{-| Creates a small test program that you can use to test your components. This is similar to
`Browser.element`.

If you set a rendering function to `viewEvents` all events from the `update` method are recorded and
shown in your HTML model. If the rendering function for `viewEvents` is `Nothing` all returned
events are discarded and ignored.

    -- don't need to be exposed. It is automaticly used by elm if you make this file directly or
    -- open it in elm reactor.
    main : Component.Test.Program MyComp MyCompMsg MyCompEvent
    main =
        Component.Test.element
            { init = init
            , view = view
            , viewEvents = Nothing
            , update = update
            , subscriptions = subscriptions
            }

-}
element :
    { init: (model, Cmd msg)
    , view: model -> Html msg
    , viewEvents: Maybe (List event -> Html Never)
    , update: msg -> model -> Triple model (Cmd msg) (List event)
    , subscriptions: model -> Sub msg
    }
    -> Program model msg event
element config =
    Browser.element
        { init = \() ->
            Tuple.mapFirst
                (\model ->
                    { model = model
                    , events = []
                    , key = Nothing
                    }
                )
            <| config.init
        , view = \comp ->
            viewEvents 
                config.viewEvents
                comp.events
            <| config.view comp.model
        , update = update config.update 
            <| config.viewEvents /= Nothing
        , subscriptions = config.subscriptions << .model
        }

{-| Creates a small test program that you can use to test your components. This is similar to
`Browser.application`.

This allows you to respond to url changes and navigate. You don't need to store the `Key` because it
is automaticly provided in the update method for you.

If you set a rendering function to `viewEvents` all events from the `update` method are recorded and
shown in your HTML model. If the rendering function for `viewEvents` is `Nothing` all returned
events are discarded and ignored.

    -- don't need to be exposed. It is automaticly used by elm if you make this file directly or
    -- open it in elm reactor.
    main : Component.Test.Program MyComp MyCompMsg MyCompEvent
    main =
        Component.Test.application
            { init = init
            , view = view
            , viewEvents = Nothing
            , update = update
            , subscriptions = subscriptions
            , onUrlRequest = onUrlRequest
            , onUrlChange = onUrlChange
            }

-}
application :
    { init: Url -> Key -> (model, Cmd msg)
    , view: model -> Html msg
    , viewEvents: Maybe (List event -> Html Never)
    , update: Key -> msg -> model -> Triple model (Cmd msg) (List event)
    , subscriptions: model -> Sub msg
    , onUrlRequest: Browser.UrlRequest -> msg
    , onUrlChange: Url.Url -> msg
    }
    -> Program model msg event
application config =
    Browser.application
        { init = \() url key ->
            Tuple.mapFirst
                (\model ->
                    { model = model
                    , events = []
                    , key = Just key
                    }
                )
            <| config.init url key
        , view = \comp ->
            { body = List.singleton
                <| viewEvents
                    config.viewEvents
                    comp.events
                <| config.view comp.model
            , title = "Component Test"
            }
        , update = \msg comp ->
            case comp.key of
                Just key -> 
                    update 
                        (config.update key)
                        (config.viewEvents /= Nothing)
                        msg
                        comp
                Nothing -> (comp, Cmd.none)
        , subscriptions = config.subscriptions << .model
        , onUrlRequest = config.onUrlRequest
        , onUrlChange = config.onUrlChange
        }

{-| Creates a small test program that you can use to test your components. This is similar to
`Browser.application`.

This allows you to navigate the page. You don't need to store the `Key` because it
is automaticly provided in the update method for you. You are not able to respond to url changes.

If you set a rendering function to `viewEvents` all events from the `update` method are recorded and
shown in your HTML model. If the rendering function for `viewEvents` is `Nothing` all returned
events are discarded and ignored.

    -- don't need to be exposed. It is automaticly used by elm if you make this file directly or
    -- open it in elm reactor.
    main : Component.Test.Program MyComp MyCompMsg MyCompEvent
    main =
        Component.Test.application
            { init = init
            , view = view
            , viewEvents = Nothing
            , update = update
            , subscriptions = subscriptions
            , noop = Noop
            }

-}
applicationNoUrl :
    { init: (model, Cmd msg)
    , view: model -> Html msg
    , viewEvents: Maybe (List event -> Html Never)
    , update: Key -> msg -> model -> Triple model (Cmd msg) (List event)
    , subscriptions: model -> Sub msg
    , noop: msg
    }
    -> Program model msg event
applicationNoUrl config =
    Browser.application
        { init = \() _ key ->
            Tuple.mapFirst
                (\model ->
                    { model = model
                    , events = []
                    , key = Just key
                    }
                )
            <| config.init
        , view = \comp ->
            { body = List.singleton
                <| viewEvents
                    config.viewEvents
                    comp.events
                <| config.view comp.model
            , title = "Component Test"
            }
        , update = \msg comp ->
            case comp.key of
                Just key -> 
                    update 
                        (config.update key)
                        (config.viewEvents /= Nothing)
                        msg
                        comp
                Nothing -> (comp, Cmd.none)
        , subscriptions = config.subscriptions << .model
        , onUrlRequest = always config.noop
        , onUrlChange = always config.noop
        }
