{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

module RPKI.Repository where

import           Control.Lens
import           Data.Generics.Product.Typed
import           Data.Ord

import           Data.X509                   (Certificate)

import           Data.Hourglass
import qualified Data.List                   as List
import           Data.List.NonEmpty          (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Map.Monoidal.Strict    (MonoidalMap(..))
import qualified Data.Map.Monoidal.Strict    as MonoidalMap
import qualified Data.Set                    as Set

import           GHC.Generics

import           RPKI.Domain
import           RPKI.Config
import           RPKI.RRDP.Types
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Time
import           RPKI.TAL
import           RPKI.Util
import           RPKI.Store.Base.Serialisation

   
data FetchStatus = Pending
                 | FetchedAt Instant
                 | FailedAt Instant  
    deriving stock (Show, Eq, Generic)    
    deriving anyclass (TheBinary)

newtype RsyncPublicationPoint = RsyncPublicationPoint { uri :: RsyncURL } 
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)

data RrdpRepository = RrdpRepository {
        uri       :: RrdpURL,
        rrdpMeta  :: Maybe RrdpMeta,
        eTag      :: Maybe ETag,
        meta      :: RepositoryMeta        
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data PublicationPoint = RrdpPP  RrdpRepository | 
                        RsyncPP RsyncPublicationPoint
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary)
   
{-   
    NOTE: This is an over-generalised version of publication point list, since the current
    RFC prescribes to only fallback from RRDP to rsync publication point, i.e. real instances 
    of this type will always consist of 1 or 2 elements.
    
    However, generic implementation is just simpler (See Fetch.hs module). 
    Whether this generic fetching procedure will be ever useful for fetching from more 
    than one RRDP links -- no idea.
-}   
newtype PublicationPointAccess = PublicationPointAccess {
        unPublicationPointAccess :: NonEmpty PublicationPoint
    }
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary)

data Repository = RrdpR RrdpRepository | 
                  RsyncR RsyncRepository
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary)

data RsyncRepository = RsyncRepository {
        repoPP :: RsyncPublicationPoint,
        meta   :: RepositoryMeta
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data PublicationPoints = PublicationPoints {
        rrdps  :: RrdpMap,
        rsyncs :: RsyncForest
    } 
    deriving stock (Show, Eq, Ord, Generic)       


data RepositoryMeta = RepositoryMeta {
        status            :: FetchStatus,
        lastFetchDuration :: Maybe TimeMs,
        refreshInterval   :: Maybe Seconds
    } 
    deriving stock (Show, Eq, Ord, Generic)   
    deriving anyclass (TheBinary)

newRepositoryMeta :: RepositoryMeta
newRepositoryMeta = RepositoryMeta {
        status            = Pending,
        lastFetchDuration = Nothing,
        refreshInterval   = Nothing
    }

newtype RrdpMap = RrdpMap { unRrdpMap :: Map RrdpURL RrdpRepository } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype Fetcheables = Fetcheables { unFetcheables :: MonoidalMap RpkiURL (Set.Set RpkiURL) }    
    deriving stock (Show, Eq, Ord, Generic)  
    deriving anyclass (TheBinary)    
    deriving newtype Monoid
    deriving newtype Semigroup

instance WithRpkiURL PublicationPoint where
    getRpkiURL (RrdpPP RrdpRepository {..}) = RrdpU uri
    getRpkiURL (RsyncPP (RsyncPublicationPoint uri)) = RsyncU uri    

instance WithRpkiURL Repository where
    getRpkiURL (RrdpR RrdpRepository {..}) = RrdpU uri
    getRpkiURL (RsyncR RsyncRepository { repoPP = RsyncPublicationPoint {..} }) = RsyncU uri

instance {-# OVERLAPPING #-} WithURL RrdpRepository where
    getURL RrdpRepository { uri = RrdpURL u } = u

instance {-# OVERLAPPING #-} WithURL RsyncRepository where
    getURL RsyncRepository { repoPP = RsyncPublicationPoint {..} } = getURL uri
    
-- TODO This is shady and I don't remember why is it like this
instance Semigroup RrdpRepository where
    r1 <> r2 = 
        if r1 ^. #meta . #status >= r2 ^. #meta . #status
            then r1 
            else r2 & #uri .~ r1 ^. #uri

-- always use the latest one
instance Ord FetchStatus where
    compare = comparing timeAndStatus
      where             
        timeAndStatus = \case 
            Pending     -> (Nothing, 0 :: Int)
            FailedAt t  -> (Just t,  0)
            FetchedAt t -> (Just t,  1) 
    
instance Semigroup RrdpMap where
    RrdpMap rs1 <> RrdpMap rs2 = RrdpMap $ Map.unionWith (<>) rs1 rs2             

getRsyncURL :: RsyncRepository -> RsyncURL
getRsyncURL RsyncRepository { repoPP = RsyncPublicationPoint { uri = url } } = url

getFetchStatus :: Repository -> FetchStatus
getFetchStatus r = getMeta r ^. #status

getMeta :: Repository -> RepositoryMeta
getMeta (RrdpR r)   = r ^. #meta
getMeta (RsyncR r)  = r ^. #meta


newRepository :: RpkiURL -> Repository
newRepository = \case   
    RrdpU u -> RrdpR $ newRrdpRepository u
    RsyncU u -> RsyncR $ newRsyncRepository u

newRrdpRepository :: RrdpURL -> RrdpRepository
newRrdpRepository uri = RrdpRepository {    
        rrdpMeta = Nothing,
        eTag     = Nothing,
        meta     = newRepositoryMeta,
        uri      = uri
    }

newRsyncRepository :: RsyncURL -> RsyncRepository
newRsyncRepository url = RsyncRepository {
        repoPP = RsyncPublicationPoint url,
        meta   = newRepositoryMeta
    }

updateMeta' :: Repository -> (RepositoryMeta -> RepositoryMeta) -> Repository
updateMeta' (RrdpR r) newMeta = RrdpR $ r & #meta %~ newMeta
updateMeta' (RsyncR r) newMeta = RsyncR $ r & #meta %~ newMeta

newFetcheables :: RpkiURL -> Maybe RpkiURL -> Fetcheables
newFetcheables primary fallback = Fetcheables $ 
    MonoidalMap.singleton primary (maybe Set.empty Set.singleton fallback)

rsyncPP :: RsyncURL -> PublicationPoint
rrdpPP  :: RrdpURL  -> PublicationPoint
rsyncPP = RsyncPP . RsyncPublicationPoint
rrdpPP = RrdpPP . newRrdpRepository

rrdpRepository :: PublicationPoints -> RrdpURL -> Maybe RrdpRepository
rrdpRepository PublicationPoints { rrdps = RrdpMap rrdps } u = Map.lookup u rrdps        

rsyncRepository :: PublicationPoints -> RsyncURL -> Maybe RsyncRepository
rsyncRepository PublicationPoints {..} rsyncUrl = 
    (\(u, meta) -> RsyncRepository (RsyncPublicationPoint u) meta)
                    <$> lookupInRsyncForest rsyncUrl rsyncs    

repositoryFromPP :: PublicationPoints -> PublicationPoint -> Maybe Repository
repositoryFromPP pps pp = 
    case getRpkiURL pp of
        RrdpU u  -> RrdpR <$> rrdpRepository merged u
        RsyncU u -> RsyncR <$> rsyncRepository merged u
  where 
    merged = mergePP pp pps        
    

mergeRsyncPP :: RsyncPublicationPoint -> PublicationPoints -> PublicationPoints
mergeRsyncPP (RsyncPublicationPoint u) pps = 
    pps & typed @RsyncForest %~ toRsyncForest u newRepositoryMeta

mergeRrdp :: RrdpRepository -> PublicationPoints -> PublicationPoints
mergeRrdp r@RrdpRepository {..} pps =
    pps & #rrdps %~ newRrdps
  where
    newRrdps (RrdpMap rrdps) = RrdpMap $ 
        case Map.lookup uri rrdps of
            Nothing -> Map.insert uri r rrdps
            Just existing 
                | r == existing -> rrdps 
                | otherwise     -> Map.insert uri (r <> existing) rrdps                        

mergePP :: PublicationPoint -> PublicationPoints -> PublicationPoints
mergePP (RrdpPP r) = mergeRrdp r
mergePP (RsyncPP r) = mergeRsyncPP r    

    
-- | Extract repositories from URIs in TAL and in TA certificate,
-- | use some reasonable heuristics, but don't try to be very smart.
-- | Prefer RRDP to rsync for everything.
publicationPointsFromTAL :: TAL -> CaCerObject -> Either ValidationError PublicationPointAccess
publicationPointsFromTAL tal (cwsX509certificate . getCertWithSignature -> cert) = 
    case tal of 
        PropertiesTAL {..} -> do 

            PublicationPointAccess ppsFromCert <- getPublicationPointsFromCert cert
            
            let uniquePrefetchRepos = map (snd . fromURI) prefetchUris

            let prefetchReposToUse = 
                    List.sort [ r | r@(RrdpPP _) <- uniquePrefetchRepos ] <>
                    List.sort [ r | r@(RsyncPP _) <- uniquePrefetchRepos ]
            
            pure $ PublicationPointAccess $ 
                maybe ppsFromCert (ppsFromCert <>) $ 
                nonEmpty prefetchReposToUse

        RFC_TAL {} -> getPublicationPointsFromCert cert
  where        
    fromURI r = 
        case r of
            RrdpU u  -> (r, rrdpPP u)
            RsyncU u -> (r, rsyncPP u)           


-- | Get publication points of the certificate.
-- 
getPublicationPointsFromCertObject :: CaCerObject -> Either ValidationError PublicationPointAccess
getPublicationPointsFromCertObject = getPublicationPointsFromCert . cwsX509certificate . getCertWithSignature

getPublicationPointsFromCert :: Certificate -> Either ValidationError PublicationPointAccess
getPublicationPointsFromCert cert = do 
    rrdp <- case getRrdpNotifyUri cert of 
                Just rrdpNotifyUri
                    | isRrdpURI rrdpNotifyUri -> Right [rrdpPP $ RrdpURL rrdpNotifyUri]
                    | otherwise               -> Left $ UnknownUriType rrdpNotifyUri
                Nothing -> Right []

    rsync <- case getRepositoryUri cert of 
                Just repositoryUri
                    | isRsyncURI repositoryUri -> 
                        case parseRsyncURL (unURI repositoryUri) of 
                            Left e   -> Left $ BrokenUri (unURI repositoryUri) e
                            Right rr -> Right [rsyncPP rr]                        
                    | otherwise -> Left $ UnknownUriType repositoryUri
                Nothing -> Right []

    case nonEmpty (rrdp <> rsync) of 
        Nothing -> Left CertificateDoesntHaveSIA
        Just ne -> Right $ PublicationPointAccess ne
          

-- Number of repositories
repositoryCount :: Fetcheables -> Int
repositoryCount (Fetcheables fs) = MonoidalMap.size fs


filterPPAccess :: Config -> PublicationPointAccess -> Maybe PublicationPointAccess    
filterPPAccess Config {..} ppAccess = 
    fmap PublicationPointAccess $ nonEmpty $ 
            NonEmpty.filter filter_ $ 
            unPublicationPointAccess ppAccess
  where
    filter_ = \case
        RrdpPP _  -> rrdpConf ^. #enabled
        RsyncPP _ -> rsyncConf ^. #enabled        


type RsyncNodeNormal = RsyncTree RepositoryMeta

-- Simple tree for representing rsync repositories grouped by host.
-- Every RsyncTree corresponds to a path chunk in the rsync URL. 
type RsyncForest = RsyncForestGen RepositoryMeta
type RsyncForestNoContent = RsyncForestGen ()
    
newtype RsyncForestGen a = RsyncForestGen (Map RsyncHost (RsyncTree a))
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data RsyncTree a = Leaf a
               | SubTree {
                   rsyncChildren :: Map RsyncPathChunk (RsyncTree a)
               } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newRsyncTree :: RsyncTree a
newRsyncTree = SubTree mempty

newRsyncForest :: RsyncForest
newRsyncForest = newRsyncForestGen

newRsyncForestGen :: RsyncForestGen a
newRsyncForestGen = RsyncForestGen Map.empty

toRsyncForest :: RsyncURL -> a -> RsyncForestGen a -> RsyncForestGen a
toRsyncForest (RsyncURL host path) a (RsyncForestGen byHost) = 
    RsyncForestGen $ Map.alter (Just . maybe 
        (buildRsyncTree path a) 
        (pathToRsyncTree path a)) host byHost      

pathToRsyncTree :: [RsyncPathChunk] -> a -> RsyncTree a -> RsyncTree a

pathToRsyncTree [] a (Leaf _) = Leaf a
pathToRsyncTree [] a SubTree {} = Leaf a

-- Strange case when we by some reason decide to merge
-- a deeper nested PP while there's a dowloaded  one some
-- higher level. Not supported, don't change the tree.
pathToRsyncTree _ _ (Leaf a) = Leaf a

pathToRsyncTree (u : us) a (SubTree ch) = 
    case Map.lookup u ch of
        Nothing    -> SubTree $ Map.insert u (buildRsyncTree us a) ch            
        Just child -> SubTree $ Map.insert u (pathToRsyncTree us a child) ch             

buildRsyncTree :: [RsyncPathChunk] -> a -> RsyncTree a
buildRsyncTree [] fs      = Leaf fs
buildRsyncTree (u: us) fs = SubTree $ Map.singleton u $ buildRsyncTree us fs    

lookupInRsyncForest :: RsyncURL -> RsyncForestGen a -> Maybe (RsyncURL, a)
lookupInRsyncForest (RsyncURL host path) (RsyncForestGen t) = do
    (path', a) <- lookupInRsyncTree path =<< Map.lookup host t
    pure (RsyncURL host path', a)

lookupInRsyncTree :: [RsyncPathChunk] -> RsyncTree a -> Maybe ([RsyncPathChunk], a)
lookupInRsyncTree path = meta_ path []
  where    
    meta_ _ realPath (Leaf fs) = Just (realPath, fs)
    meta_ [] _  SubTree {} = Nothing
    meta_ (u: us) realPath SubTree {..} = 
        Map.lookup u rsyncChildren >>= meta_ us (realPath <> [u])


flattenRsyncTree :: RsyncForestGen a -> [(RsyncURL, a)]
flattenRsyncTree (RsyncForestGen t) = 
    concatMap (uncurry flattenTree) $ Map.toList t    

flattenTree :: RsyncHost -> RsyncTree a -> [(RsyncURL, a)]
flattenTree host tree = go tree []
  where 
    go (Leaf info) realPath  = [(RsyncURL host (reverse realPath), info)]
    go SubTree {..} realPath = 
        concatMap (\(p, n) -> go n (p : realPath)) $ Map.toList rsyncChildren  
