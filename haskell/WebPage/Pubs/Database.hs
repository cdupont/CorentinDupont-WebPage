{-# LANGUAGE PatternGuards, TupleSections #-}

module WebPage.Pubs.Database where

import WebPage.Pubs.Paper


--
-- * Defintions
--Dang, M.-Q., Basmadjian, R., De Meer, H., Lent, R., Mahmoodi, T., Sannelli, D., Mezza, F., Dupont,

-- ** Authors
dupont      = Author "Corentin" "Dupont"
hermenier   = Author "Fabien" "Hermenier"
schultze    = Author "Thomas" "Shultze"
somov       = Author "Andrey" "Somov"
giuliani    = Author "Giovani" "Guiliani"
quan        = Author "Quan" "Minh Dang"
demeer      = Author "Hermann" "De Meer"
basmadjian  = Author "Robert" "Basmadjian"
lent        = Author "Ricardo" "Lent"
sannelli    = Author "Domenico" "Sannelli"
mezza       = Author "Federico" "Mezza"
mahmoodi    = Author "Totkam" "Mahmoodi"
giaffreda   = Author "Raffaele" "Giaffreda"

-- ** Institutions
unitn = "University of Trento"

-- ** Journals

-- ** Conferences
eenergy   = short "E-Energy" "International Conference on Future Energy Systems"
iscis     = short "ISCIS" "International Symposium on Computer and Information Sciences"
funems     = short "FUNEMS" "Future Network and Mobile Summit"
-- ** Workshops
e2dc  = short "E2DC" "International Workshop on Energy-Efficient Data Centres"


--
-- * Papers
--

-- ** Lists of papers in chronological order.

--drafts = [v]
y11 = [iscis11]
y12 = [eenergy12,e2dc12]
y13 = [e2dc13, funems13]

allPubs = concat [y11, y12, y13]


-- ** Under Review

--v = draft
--  "XX-variational-data"
--  [walkingshaw,kaestner,erwig,apel,bodden]
--  "Variational Data Structures: Exploring Trade-Offs in Computing With Variability"
--  2014

iscis11= conference
  "ISCIS11"
  [quan, basmadjian, demeer, lent, mahmoodi, sannelli, mezza, dupont]
  "Energy Efficient Resource Allocation Strategy for Cloud Data Centres"
  2011
  `onPages` Pages 133 141
  @@ iscis

eenergy12= conference
  "EENERGY12"
  [dupont, giuliani, hermenier, schultze, somov]
  "An Energy Aware Framework for Virtual Machine Placement in Cloud federated data centres"
  2012
  `onPages` Pages 1 10
  @@ eenergy

e2dc12 = workshop
  "E2DC12"
  [quan, dupont, somov]
  "Energy Usage and Carbon Emission Optimization Mechanism for Federated Data Centres"
  2012
  `onPages` Pages 129 140
  @@ e2dc

e2dc13 = workshop
  "E2DC13"
  [dupont]
  "Renewable Energy Aware Data Centres: The Problem of Controlling the Applications Workload"
  2013
  @@ e2dc

funems13= conference
  "FUNEMS13"
  [somov, dupont, giaffreda]
  "Smart-City Mobility with Cognitive Internet of Things"
  2013
  `onPages` Pages 1 10
  @@ funems
