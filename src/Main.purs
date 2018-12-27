module Main where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.Traversable (traverse)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (modify_, new, read, write)
import Prim.Row (class Union)
import Web.DOM.Document (createElement, Document, createTextNode)
import Web.DOM.Document (toNode, toParentNode) as D
import Web.DOM.Element (Element, fromNode, setAttribute, toNode, toParentNode)
import Web.DOM.MutationObserver (MutationObserver, MutationObserverInitFields, mutationObserver, observe)
import Web.DOM.MutationRecord (MutationRecord)
import Web.DOM.Node (Node, appendChild, firstChild, setTextContent, textContent)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.DOM.Text (toNode) as T
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument) as HD
import Web.HTML.HTMLElement (toElement) as HE
import Web.HTML.Window (document)
foreign import loadTwitterImpl :: Fn1 Unit (Effect Unit)

loadTwitter :: Effect Unit
loadTwitter = runFn1 loadTwitterImpl unit

type MayDom a = MaybeT Effect a


runMayDom :: forall a. MaybeT Effect a -> Effect (Maybe a)
runMayDom  = runMaybeT 

runMayDomUnit :: MaybeT Effect Unit -> Effect Unit
runMayDomUnit  = runMaybeT >>> (pure unit <* _)

changeTableCss :: Document -> Effect Unit
changeTableCss doc = runMayDomUnit do
  let documentP = D.toParentNode doc
  table <- MaybeT $ querySelector (QuerySelector ".table_style02") documentP
  traceM table
  liftEffect $ setAttribute "style" "table-layout: fixed;" table
  titleTh <- MaybeT $ querySelector (QuerySelector "th.title") documentP
  traceM titleTh
  liftEffect $ setAttribute "style" "width: 32%;" titleTh
  pure unit
  

appendTd :: Document -> Node -> Effect Unit
appendTd d tr = runMayDomUnit do
  trElm <- MaybeT $ pure $ fromNode tr
  trElmParent <- pure $ toParentNode trElm
  titleElem <- MaybeT $ querySelector (QuerySelector "td:nth-child(2)") trElmParent
  artistElem <- MaybeT $ querySelector (QuerySelector "td:nth-child(3)") trElmParent
  artist <- liftEffect <<< map trim <<< textContent <<< toNode $ artistElem
  artistTextNode <- MaybeT $ firstChild (toNode artistElem)
  liftEffect $ setTextContent "" artistTextNode
  artistP <- liftEffect $ createElement "p" d
  liftEffect $ setAttribute "style" "width: calc(100% - 150px)" artistP
  artistPNode <- pure $ toNode artistP
  liftEffect $ setTextContent artist artistPNode 
  _ <- liftEffect $ appendChild artistPNode (toNode artistElem)
  title <- liftEffect $ textContent $ toNode titleElem
  tdElm <- liftEffect $ createElement "td" d
  liftEffect $ setAttribute "style" "width: 10%;" tdElm 
  tdNode <- pure $ toNode tdElm
  let tweet = createTweet title artist
  _ <- liftEffect $ createTweetButton tweet d >>= (_ `appendChild` tdNode) 
  _ <- liftEffect $ appendChild tdNode tr
  pure unit

createTweet :: String -> String -> String
createTweet title artist = "♪Now Listening \n" <> title <> " / " <> artist <> "\n"

appendTdHead :: Document -> Node ->Effect Unit
appendTdHead d tr = do
  thElm <- createElement "th" d
  setAttribute "style" "width: 10%;" thElm 
  let thNode = toNode thElm
  _ <- createTextNode "Tweet!" d >>= T.toNode >>> (_ `appendChild` thNode)
  appendChild thNode tr *> pure unit

type MutationObserverRet = {record :: MutationRecord, observer :: MutationObserver}

runMutationObserver :: 
  forall r rx. Union r rx MutationObserverInitFields => {|r} -> Node -> (MutationObserverRet -> Effect Unit) -> Effect Unit
runMutationObserver init node cb = do
  observer <- mutationObserver \r o -> do
    cb {record : r, observer : o}
  observe node init observer

addTweetWidget :: Document -> Element -> Effect Element
addTweetWidget d body = do
  traceM body
  elm <- createElement "script" d
  setAttribute "src" "https://platform.twitter.com/widgets.js" elm
  setAttribute "charset" "utf-8" elm
  setAttribute "async" "async" elm
  _ <- appendChild (toNode elm) (toNode body)
  pure elm

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
  mb <- window >>= document >>= HD.body
  case HE.toElement <$> mb of
    Nothing -> pure unit
    Just body -> do
      doc <- window >>= document >>= HD.toDocument >>> pure
      let
        documentP = D.toParentNode doc
        documentN = D.toNode doc
        init = {childList : true}
      mayTop <- querySelector (QuerySelector "#noa-container") documentP
      case mayTop of
        Nothing -> pure unit
        Just top -> do
          let topNode = toNode top
          let topParentNode = toParentNode top
          flag <- new true
          loadScript <- new false
          runMutationObserver init topNode \obsRet -> do
              traceM (obsRet.record)
              flagVal <- read flag
              case flagVal of
                true -> do
                  changeTableCss doc
                  modify_ (const false) flag
                  headTr <- querySelector (QuerySelector "thead tr") topParentNode
                  case headTr of
                    Nothing -> write true flag *> pure unit
                    Just tr -> do 
                      appendTdHead doc (toNode tr)
                      trs <- 
                        querySelectorAll (QuerySelector "tbody tr") topParentNode 
                          >>= toArray
                      _ <- traverse (appendTd doc) trs
                      write true flag
                      loads <- read loadScript
                      case loads of
                        true ->
                          loadTwitter
                        false -> do
                          _ <- addTweetWidget doc body
                          write true loadScript
                      pure unit
                false ->
                  pure unit
          pure unit
