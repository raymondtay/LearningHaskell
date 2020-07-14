
module MovieReview where

import Control.Monad


data MovieReview = MovieReview {
  revTitle:: String,
  revUser :: String,
  revReview :: String
  }


-- a naive function
-- it returns a MovieReview only if the alist contains all of the necessary
-- values, and they ar enot all non-empty stirngs. However, the fact that it
-- validates its inputs isits only merit. It suffers badly from the staircasing
-- that we have learnt to be wary of, and it knows the intimate details of the
-- representation of an alist.
simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist =
  case lookup "title" alist of
      Just (Just title@(_:_)) -> 
        case lookup "user" alist of
            Just (Just user@(_:_)) ->
              case lookup "review" alist of 
                  Just (Just review@(_:_)) ->
                    Just (MovieReview title user review)
                  _ -> Nothing -- no review
            _ -> Nothing -- no user
      _ -> Nothing -- no title

-- let's write a better version of the previous function, simpleReview and call
-- it maybeReview
maybeReview alist = do
  title <- lookup1 "title" alist
  user <- lookup1 "user" alist
  review <- lookup1 "review" alist
  return (MovieReview title user review)

lookup1 key alist = case lookup key alist of
                        Just (Just x@(_:_)) -> Just x
                        _ -> Nothing

-- this above function, maybeReview is already better and the natural question
-- to ask is: can we do better?
--
liftedReview alist = 
  liftM3 MovieReview (lookup1 "title" alist)
                     (lookup1 "user" alist)
                     (lookup1 "review" alist)
                   
apReview alist =
  MovieReview `liftM` lookup1 "title" alist
                 `ap` lookup1 "user" alist
                 `ap` lookup1 "review" alist
                


