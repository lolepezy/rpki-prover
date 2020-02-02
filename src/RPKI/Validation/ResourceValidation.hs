{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE NumericUnderscores #-}
module RPKI.Validation.ResourceValidation where

import Common.SmallSet (SmallSet)
import qualified Common.SmallSet as SmallSet

import           RPKI.AppMonad
import           RPKI.Errors
import           RPKI.Resources.Resources

validateChildParentResources :: ValidationRFC -> 
                                AllResources -> 
                                AllResources -> 
                                Maybe (VerifiedRS PrefixesAndAsns) -> 
                                PureValidator conf (VerifiedRS PrefixesAndAsns)
validateChildParentResources validationRFC childResources parentResources verifiedResources =                                 
  case validationRFC of 
    Strict_       -> verify strict
    Reconsidered_ -> verify reconsidered
  where    
    verify f = do 
      c4 <- check childIpv4s parentIpv4s (\(VerifiedRS (PrefixesAndAsns r _ _)) -> r)
      c6 <- check childIpv6s parentIpv6s (\(VerifiedRS (PrefixesAndAsns _ r _)) -> r)
      ca <- check childAsns parentAsns (\(VerifiedRS (PrefixesAndAsns _ _ r)) -> r)
      f c4 c6 ca

    check :: (Eq a, WithSetOps a) => 
            (RSet (SmallSet a)) -> 
            (RSet (SmallSet a)) -> 
            (VerifiedRS PrefixesAndAsns -> SmallSet a) -> 
            PureValidator conf (ResourceCheckResult a)
    check c p verifiedSub = 
      case verifiedResources of 
        Nothing -> do 
          case (c, p) of 
            (_,       Inherit) -> pureError InheritWithoutParentResources
            (Inherit, RS ps)   -> pure $ Left $ Nested ps
            (RS cs,   RS ps)   -> pure $ subsetCheck cs ps
        Just vr -> 
          pure $ case (c, p) of 
            (Inherit, Inherit) -> Left $ Nested (verifiedSub vr)
            (RS cs,   Inherit) -> subsetCheck cs (verifiedSub vr)
            (Inherit, RS _)    -> Left $ Nested (verifiedSub vr)
            (RS cs,   RS _)    -> subsetCheck cs (verifiedSub vr)

    strict q4 q6 qa = 
      case (q4, q6, qa) of
        (Left (Nested n4), Left (Nested n6), Left (Nested na)) -> 
          pure $ VerifiedRS $ PrefixesAndAsns n4 n6 na
        _ -> pureError $ OverclaimedResources $ 
          PrefixesAndAsns (overclaimed q4) (overclaimed q6) (overclaimed qa)
 
    reconsidered q4 q6 qa = do
      case (q4, q6, qa) of
        (Left (Nested _), Left (Nested _), Left (Nested _)) -> 
          pure ()
        _ -> 
          pureWarning $ ValidationWarning $ ValidationE $ OverclaimedResources $
            PrefixesAndAsns (overclaimed q4) (overclaimed q6) (overclaimed qa)
      pure $ VerifiedRS $ PrefixesAndAsns (nested q4) (nested q6) (nested qa)          

    AllResources childIpv4s childIpv6s childAsns = childResources
    AllResources parentIpv4s parentIpv6s parentAsns = parentResources

    overclaimed (Left _) = SmallSet.empty
    overclaimed (Right (_, Overclaiming o)) = o

    nested (Left (Nested n)) = n
    nested (Right (Nested n, _)) = n

