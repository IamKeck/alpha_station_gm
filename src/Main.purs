module Main where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (new, read, modify_)
import Prim.Row (class Union)
import Web.DOM.Document (createElement, Document, createTextNode)
import Web.DOM.Document (toNode, toParentNode) as D
import Web.DOM.Element (Element, fromNode, setAttribute, toNode, toParentNode)
import Web.DOM.MutationObserver (MutationObserver, MutationObserverInitFields, mutationObserver, observe)
import Web.DOM.MutationRecord (MutationRecord)
import Web.DOM.Node (Node, appendChild, textContent, toEventTarget)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.DOM.Text (toNode) as T
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument) as HD
import Web.HTML.HTMLElement (toElement) as HE
import Web.HTML.Window (document)

type MayDom a = MaybeT Effect a

runMayDom :: forall a. MaybeT Effect a -> Effect (Maybe a)
runMayDom  = runMaybeT 

runMayDomUnit :: MaybeT Effect Unit -> Effect Unit
runMayDomUnit  = runMaybeT >>> (pure unit <* _)

searchInput :: QuerySelector
searchInput = QuerySelector "input[name='q']"

appendTd :: Document -> Node -> Effect Unit
appendTd d tr = runMayDomUnit do
  _ <- liftEffect $ log ("appendtd!!!!!!")
  trElm <- MaybeT $ pure $ fromNode tr
  trElmParent <- pure $ toParentNode trElm
  titleElem <- MaybeT $ querySelector (QuerySelector "td:nth-child(1)") trElmParent
  artistElem <- MaybeT $ querySelector (QuerySelector "td:nth-child(2)") trElmParent
  artist <- liftEffect $ textContent $ toNode artistElem
  title <- liftEffect $ textContent $ toNode titleElem
  tdNode <- liftEffect $ toNode <$> createElement "td" d
  let tweet = createTweet title artist
  _ <- liftEffect $ createTweetButton tweet d >>= (_ `appendChild` tdNode) 
  _ <- liftEffect $ appendChild tdNode tr
  pure unit

createTweet :: String -> String -> String
createTweet title artist = "♪Now Listening \n" <> title <> " / " <> artist <> "\n"

appendTdHead :: Document -> Node ->Effect Unit
appendTdHead d tr = do
  log ("appendtdhead!!!!!!")
  tdNode <- toNode <$> createElement "th" d
  _ <- createTextNode "Tweet!" d >>= T.toNode >>> (_ `appendChild` tdNode)
  appendChild tdNode tr *> pure unit

type MutationObserberAffRet = {record :: MutationRecord, observer :: MutationObserver}

mutationObserverAff :: 
  forall r rx. Union r rx MutationObserverInitFields => {|r} -> Node -> Aff MutationObserberAffRet
mutationObserverAff init node = makeAff $ \next -> do
  log ("changed!!!!!!")
  let callback = \r o -> {record : r, observer : o} # Right >>> next
  observer <- mutationObserver callback
  observe node init observer
  pure mempty

addTweetWidget :: Document -> Element -> Effect Node
addTweetWidget d body = do
  traceM body
  elm <- createElement "script" d
  setAttribute "src" "https://platform.twitter.com/widgets.js" elm
  setAttribute "charset" "utf-8" elm
  setAttribute "async" "async" elm
  _ <- appendChild (toNode elm) (toNode body)
  pure (toNode elm)

createTweetButton :: String -> Document -> Effect Node
createTweetButton t d = do
  elm <- createElement "a" d
  setAttribute "href" "https://twitter.com/share?ref_src=twsrc%5Etfw" elm
  setAttribute "class" "twitter-share-button" elm
  setAttribute "data-text" t elm
  setAttribute "data-show-count" "false" elm
  setAttribute "data-url" " " elm
  setAttribute "data-hashtags" "alphastation" elm
  textNode <- T.toNode <$> createTextNode "Tweet!" d 
  let elmNode = toNode elm
  appendChild textNode elmNode *> pure elmNode

onAff :: Node -> String -> Aff Event
onAff n evt = makeAff \next -> do 
  listner <- eventListener \e -> next $ Right e
  addEventListener eventType listner true eventTarget
  pure mempty
  where
    eventType = EventType evt
    eventTarget = toEventTarget n

main :: Effect Unit
main = do
  log "script first"
  mb <- window >>= document >>= HD.body
  case HE.toElement <$> mb of
    Nothing -> pure unit
    Just body -> do
      doc <- window >>= document >>= HD.toDocument >>> pure
      let
        documentP = D.toParentNode doc
        documentN = D.toNode doc
        init = {childList : true, characterData: true, attributes: true}
      scriptNode <- addTweetWidget doc body
      mayTop <- querySelector (QuerySelector "#noa-container") documentP
      case mayTop of
        Nothing -> log "no-container" *> pure unit
        Just top -> do
          let topNode = toNode top
          let topParentNode = toParentNode top
          flag <- new true
          _ <- launchAff $ do
              obsRet <- mutationObserverAff init topNode
              liftEffect do
                traceM (obsRet.record)
                flagVal <- read flag
                case flagVal of
                  true -> do
                    modify_ (const false) flag
                    headTr <- querySelector (QuerySelector "thead tr") topParentNode
                    case headTr of
                      Nothing -> modify_ (const true) flag *> pure unit
                      Just tr -> do 
                        appendTdHead doc (toNode tr)
                        trs <- 
                          querySelectorAll (QuerySelector "tbody tr") topParentNode 
                            >>= toArray
                        _ <- traverse (appendTd doc) trs
                        modify_ (const true) flag
                        pure unit
                  false ->
                    pure unit
          pure unit
