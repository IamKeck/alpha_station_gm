module Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (length)
import Data.List.NonEmpty (elemLastIndex)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Traversable (sequence, traverse)
import Debug.Trace (trace, traceM)
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
import Web.DOM.Node (Node, appendChild, parentNode, textContent)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.DOM.Text (toNode) as T
import Web.HTML (window)
import Web.HTML.Event.EventTypes (offline)
import Web.HTML.HTMLDocument (body, toDocument) as HD
import Web.HTML.HTMLElement (toElement) as HE
import Web.HTML.Window (document)

searchInput :: QuerySelector
searchInput = QuerySelector "input[name='q']"

appendTd :: Document -> Node -> Effect Unit
appendTd d tr = do
  log ("appendtd!!!!!!")
  let trElm = fromNode tr
  case toParentNode <$> trElm of
    Nothing -> pure unit
    Just trParent -> do
      titleElemM <- querySelector (QuerySelector "td:nth-child(1)") trParent
      artistElemM <- querySelector (QuerySelector "td:nth-child(2)") trParent
      case titleElemM of
        Nothing -> pure unit
        Just titleElem ->
          case artistElemM of
            Nothing -> pure unit
            Just artistElem -> do
              artist <- textContent $ toNode artistElem
              title <- textContent $ toNode titleElem
              tdNode <- toNode <$> createElement "td" d
              let tweet = createTweet title artist
              _ <- createTweetButton tweet d >>= (_ `appendChild` tdNode) 
              _ <- appendChild tdNode tr
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
mutationObserverAff init node = makeAff $ \resulter -> do
  log ("changed!!!!!!")
  let callback = \r o -> {record : r, observer : o} # Right >>> resulter
  observer <- mutationObserver callback
  observe node init observer
  pure mempty

addTweetWidget :: Document -> Element -> Effect Unit
addTweetWidget d body = do
  traceM body
  elm <- createElement "script" d
  setAttribute "src" "https://platform.twitter.com/widgets.js" elm
  setAttribute "charset" "utf-8" elm
  -- setAttribute "async" "async" elm
  appendChild (toNode elm) (toNode body) *> pure unit

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
      addTweetWidget doc body
      mayTop <- querySelector (QuerySelector "#noa-container") documentP
      case mayTop of
        Nothing -> log "no-container" *> pure unit
        Just top -> do
          let topNode = toNode top
          let topParentNode = toParentNode top
          flag <- new true
          _ <- launchAff $ do
              obsRet <- mutationObserverAff init topNode
              traceM (obsRet.record)
              flagVal <- liftEffect $ read flag
              case flagVal of
                true -> do
                  liftEffect $ modify_ (const false) flag
                  headTr <- liftEffect $ querySelector (QuerySelector "thead tr") topParentNode
                  case headTr of
                    Nothing -> (liftEffect $ modify_ (const true) flag) *> pure unit
                    Just tr -> do 
                      liftEffect $ appendTdHead doc (toNode tr)
                      trsn <- liftEffect (querySelectorAll (QuerySelector "tbody tr") topParentNode)
                      trs <- (liftEffect $ toArray trsn)  :: Aff (Array Node)
                      traceM trs
                      a <- for trs (\tr2 -> do
                        traceM tr2
                        liftEffect $ appendTd doc tr2
                        pure unit
                      )
                      liftEffect $ modify_ (const true) flag
                      pure unit
                      
                false ->
                  pure unit
          pure unit
