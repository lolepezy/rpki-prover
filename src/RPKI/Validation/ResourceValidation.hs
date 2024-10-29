module RPKI.Validation.ResourceValidation where

import           RPKI.Domain
import           RPKI.AppMonad
import           RPKI.Reporting
import qualified RPKI.Resources.IntervalSet as IS
import           RPKI.Resources.Types


-- | Validate two sets of resources with two options: 
--
-- 'strict' validation (https://tools.ietf.org/html/rfc6487#page-17) 
-- verifies that child resources are nested into parent's.
--
-- 'reconsidered' (https://tools.ietf.org/html/rfc8360) verifies that child 
-- resources are nested into verifiedResources that are, in general case, a 
-- subset of parent resources.
-- 
validateChildParentResources :: 
             ValidationRFC 
          -> AllResources 
          -> AllResources 
          -> Maybe (VerifiedRS PrefixesAndAsns) 
          -> PureValidatorT (VerifiedRS PrefixesAndAsns, Maybe (Overclaiming PrefixesAndAsns))
validateChildParentResources validationRFC childResources parentResources verifiedResources =                                 
  case validationRFC of 
    StrictRFC       -> verify strict
    ReconsideredRFC -> verify reconsidered
  where    
    verify validateBasedOnRFC = do 
      c4 <- check childIpv4s parentIpv4s (\(VerifiedRS (PrefixesAndAsns r _ _)) -> r)
      c6 <- check childIpv6s parentIpv6s (\(VerifiedRS (PrefixesAndAsns _ r _)) -> r)
      ca <- check childAsns parentAsns (\(VerifiedRS (PrefixesAndAsns _ _ r)) -> r)
      validateBasedOnRFC c4 c6 ca

    check :: Interval a =>               
            RSet (IntervalSet a) -> 
            RSet (IntervalSet a) -> 
            (VerifiedRS PrefixesAndAsns -> IntervalSet a) -> 
            PureValidatorT (IS.ResourceCheckResult a)
    check c p verifiedSub = 
      case verifiedResources of 
        Nothing -> 
          case (c, p) of 
            (_,       Inherit) -> vPureError InheritWithoutParentResources
            (Inherit, RS ps)   -> pure $ Left $ Nested ps
            (RS cs,   RS ps)   -> pure $ IS.subsetCheck cs ps
        Just vr -> 
          pure $! case (c, p) of 
            (Inherit, Inherit) -> Left $ Nested (verifiedSub vr)
            (RS cs,   Inherit) -> IS.subsetCheck cs (verifiedSub vr)
            (Inherit, RS _)    -> Left $ Nested (verifiedSub vr)
            (RS cs,   RS _)    -> IS.subsetCheck cs (verifiedSub vr)

    strict q4 q6 qa = 
      case (q4, q6, qa) of
        (Left (Nested n4), Left (Nested n6), Left (Nested na)) -> 
          pure (VerifiedRS (PrefixesAndAsns n4 n6 na), Nothing)
        _ -> vPureError $ OverclaimedResources $ 
          PrefixesAndAsns (overclaimed q4) (overclaimed q6) (overclaimed qa)
 
    reconsidered q4 q6 qa = do      
      case (q4, q6, qa) of
        (Left (Nested _), Left (Nested _), Left (Nested _)) -> 
          pure (VerifiedRS (PrefixesAndAsns (nested q4) (nested q6) (nested qa)), Nothing)
        _ -> do
          pureWarning $ VWarning $ ValidationE $ OverclaimedResources $
            PrefixesAndAsns (overclaimed q4) (overclaimed q6) (overclaimed qa)
          pure (VerifiedRS (PrefixesAndAsns (nested q4) (nested q6) (nested qa)),
                Just $ Overclaiming $ PrefixesAndAsns (overclaimed q4) (overclaimed q6) (overclaimed qa))          

    AllResources childIpv4s childIpv6s childAsns = childResources
    AllResources parentIpv4s parentIpv6s parentAsns = parentResources

overclaimed :: Either a (b, Overclaiming (IntervalSet c)) -> IntervalSet c
overclaimed (Left _) = IS.empty
overclaimed (Right (_, Overclaiming o)) = o

nested :: Either (Nested p) (Nested p, b) -> p
nested (Left (Nested n)) = n
nested (Right (Nested n, _)) = n


validateNested :: PrefixesAndAsns -> PrefixesAndAsns -> PureValidatorT ()
validateNested (PrefixesAndAsns i4 i6 ia) (PrefixesAndAsns o4 o6 oa) = do 
    let i4c = IS.subsetCheck i4 o4
    let i6c = IS.subsetCheck i6 o6
    let ac  = IS.subsetCheck ia oa
    case (i4c, i6c, ac) of
        (Left (Nested _), Left (Nested _), Left (Nested _)) -> pure ()
        _ -> vPureError $ OverclaimedResources $ 
                PrefixesAndAsns (overclaimed i4c) (overclaimed i6c) (overclaimed ac)

