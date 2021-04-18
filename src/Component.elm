module Component exposing 
    ( update, updateEvents
    )

{-| This library contains some function that make the update and event process with components
easier.

# Update Handlers
@docs update, updateEvents

-}

import Triple exposing (Triple)


{-| Performs the update and handling the events of a component. The event handler can generate new
events which can propagated to the host of the host.

    import MyComponent -- the client component
    import Component -- this library

    update : Msg -> Host -> (Host, Cmd Msg, List Event)
    update msg host =
        case msg of
            ...
            -- call the update manager
            WrapMyComponent compMsg ->
                Component.updateEvents
                    MyComponent.update
                    handleMyCompEvents
                    .component
                    (\c h -> { h | component = c })
                    WrapMyComponent
                    compMsg
                    host
            ...

    -- handle all possible events. New events can be generated here.
    handleMyCompEvents : Component.Event -> Host -> (Host, Cmd Msg, List Event)
    handleMyCompEvents event host =
        case event of
            ...
            MyComponent.Event1 -> ...
            ...

-}
updateEvents : (cmsg -> comp -> Triple comp (Cmd cmsg) (List cevent))
    -> (cevent -> model -> Triple model (Cmd msg) (List event))
    -> (model -> comp)
    -> (comp -> model -> model)
    -> (cmsg -> msg)
    -> cmsg
    -> model
    -> Triple model (Cmd msg) (List event)
updateEvents updateComponent handleEvents getComponent setComponent wrapComponent componentMsg model =
    let

        updatedComp : Triple model (Cmd msg) (List cevent)
        updatedComp =
            Triple.mapFirst 
                (\c -> setComponent c model)
            <| Triple.mapSecond
                (Cmd.map wrapComponent)
            <| updateComponent
                componentMsg
            <| getComponent model
        
        handledEvents : Triple model (List (Cmd msg)) (List event)
        handledEvents =
            List.foldl
                (\event carry ->
                    Triple.mapSecond
                        (\c -> c :: Triple.second carry)
                    <| Triple.mapThird
                        (\c -> c ++ Triple.third carry)
                    <| handleEvents
                        event
                    <| Triple.first carry
                )
                (Triple.first updatedComp, [], [])
            <| Triple.third updatedComp
        
    in 
        Triple.mapSecond
            (Cmd.batch << (::) (Triple.second updatedComp))
            handledEvents

{-| Performs the update and handling the events of a component. In the handling of the events
no new events are generated.

    import MyComponent -- the client component
    import Component -- this library

    update : Msg -> Host -> (Host, Cmd Msg)
    update msg host =
        case msg of
            ...
            -- call the update manager
            WrapMyComponent compMsg ->
                Component.update
                    MyComponent.update
                    handleMyCompEvents
                    .component
                    (\c h -> { h | component = c })
                    WrapMyComponent
                    compMsg
                    host
            ...

    -- handle all possible events
    handleMyCompEvents : Component.Event -> Host -> (Host, Cmd Msg)
    handleMyCompEvents event host =
        case event of
            ...
            MyComponent.Event1 -> ...
            ...

-}
update : (cmsg -> comp -> Triple comp (Cmd cmsg) (List cevent))
    -> (cevent -> model -> (model, Cmd msg))
    -> (model -> comp)
    -> (comp -> model -> model)
    -> (cmsg -> msg)
    -> cmsg
    -> model
    -> (model, Cmd msg)
update updateComponent handleEvents getComponent setComponent wrapComponent componentMsg model =
    Triple.dropThird
    <| updateEvents
        updateComponent
        (\ev mod ->
            Triple.insertThird []
            <| handleEvents ev mod
        )
        getComponent
        setComponent
        wrapComponent
        componentMsg
        model
