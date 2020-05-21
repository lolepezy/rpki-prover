{-# LANGUAGE NumericUnderscores #-}

module RPKI.Validation.ResourceValidation where

import           RPKI.AppMonad
import           RPKI.Errors
import qualified RPKI.Resources.IntervalSet as IS
import           RPKI.Resources.Types

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

    check :: Interval a => 
            RSet (IntervalSet a) -> 
            RSet (IntervalSet a) -> 
            (VerifiedRS PrefixesAndAsns -> IntervalSet a) -> 
            PureValidator conf (IS.ResourceCheckResult a)
    check c p verifiedSub = 
      case verifiedResources of 
        Nothing -> 
          case (c, p) of 
            (_,       Inherit) -> vPureError InheritWithoutParentResources
            (Inherit, RS ps)   -> pure $ Left $ Nested ps
            (RS cs,   RS ps)   -> pure $ IS.subsetCheck cs ps
        Just vr -> 
          pure $ case (c, p) of 
            (Inherit, Inherit) -> Left $ Nested (verifiedSub vr)
            (RS cs,   Inherit) -> IS.subsetCheck cs (verifiedSub vr)
            (Inherit, RS _)    -> Left $ Nested (verifiedSub vr)
            (RS cs,   RS _)    -> IS.subsetCheck cs (verifiedSub vr)

    strict q4 q6 qa = 
      case (q4, q6, qa) of
        (Left (Nested n4), Left (Nested n6), Left (Nested na)) -> 
          pure $ VerifiedRS $ PrefixesAndAsns n4 n6 na
        _ -> vPureError $ OverclaimedResources $ 
          PrefixesAndAsns (overclaimed q4) (overclaimed q6) (overclaimed qa)
 
    reconsidered q4 q6 qa = do
      case (q4, q6, qa) of
        (Left (Nested _), Left (Nested _), Left (Nested _)) -> 
          pure ()
        _ -> 
          pureWarning $ VWarning $ ValidationE $ OverclaimedResources $
            PrefixesAndAsns (overclaimed q4) (overclaimed q6) (overclaimed qa)
      pure $ VerifiedRS $ PrefixesAndAsns (nested q4) (nested q6) (nested qa)          

    AllResources childIpv4s childIpv6s childAsns = childResources
    AllResources parentIpv4s parentIpv6s parentAsns = parentResources

    overclaimed (Left _) = IS.empty
    overclaimed (Right (_, Overclaiming o)) = o

    nested (Left (Nested n)) = n
    nested (Right (Nested n, _)) = n

