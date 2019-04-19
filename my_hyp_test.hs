
import System.Random


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs


--
-- Типы графов
--
-- Бесконтекстный L-граф:
-- алфавит Sigma [Char], D-множество E(P) [Bracket], множество вершин V [Vertice], /*( E(P) = Left(P) U Right(P) U /empty/ )*/
-- функция дуг ArcFunc [Arc] ( Arc = V x E(P) x (Sigma U /empty/) x V ), начальная вершина P0 Vertice, множество конечных вершин F [Vertice]
--
-- Недетерменированный конечный автомат (НКА):
-- алфавит Sigma [Char], множество вершин V [Vertice], функция дуг ArcFunc [NFAArc] ( NFAArc = V x (Sigma U /empty/) x V ),
-- начальная вершина P0 Vertice, множество конечных вершин F [Vertice]
--

data OpCl = Open | Close
	deriving(Eq, Show)

data Bracket = Bracket Int OpCl
	deriving(Eq, Show)

data Vertice = Vertice Int (Maybe Bracket)
	deriving(Eq, Show)

data Arc = LArc Vertice (Maybe Bracket) (Maybe Char) Vertice | NFAArc Vertice (Maybe Char) Vertice
	deriving(Eq, Show)



data Graph = L_graph [Char] [Bracket] [Vertice] [Arc] Vertice [Vertice] |
			 NFA [Char] [Vertice] [Arc] Vertice [Vertice]
	deriving(Eq, Show)







tuple_first :: (a, b, c) -> a
tuple_first (a, _, _) = a

tuple_second :: (a, b, c) -> b
tuple_second (_, b, _) = b

tuple_third :: (a, b, c) -> c
tuple_third (_, _, c) = c

i_elem :: [a] -> Int -> a
i_elem (a:_) 0 = a
i_elem (_:as) n = i_elem as (n-1)

fst_vtc :: Arc -> Vertice
fst_vtc (LArc v _ _ _) = v
fst_vtc (NFAArc v _ _) = v

snd_vtc :: Arc -> Vertice
snd_vtc (LArc _ _ _ v) = v
snd_vtc (NFAArc _ _ v) = v

char :: Arc -> Maybe Char
char (LArc _ _ c _) = c
char (NFAArc _ c _) = c

empty :: [a] -> Bool
empty [] = True
empty _ = False



first :: VASF -> [Vertice]
first (VASF vertices _ _ _) = vertices

second :: VASF -> [Arc]
second (VASF _ arcs _ _) = arcs

third :: VASF -> Vertice
third (VASF _ _ start _) = start

fourth :: VASF -> [Vertice]
fourth (VASF _ _ _ finished) = finished





vert_arcs :: Vertice -> [Arc] -> [Arc]
vert_arcs _ [] = []
vert_arcs v (a:as)	| (fst_vtc a) == v	= a:(vert_arcs v as)
					| otherwise			= vert_arcs v as


-- Построить список списков дуг с началом в каждом состоянии
-- Построить список чисел для каждого состояния (от 0 (никуда не уходит) до кол-ва дуг)
-- Начать в начальной вершине, пройти по дугам с соотв. номерами в списке (0 - путь закончился)
-- НЕ РАБОТАЕТ!!!



lines :: Vertice -> [Vertice] -> [Arc] -> Int -> [Int] -> [[Arc]]
lines _ _ _ _ _ = 





test_l_graph :: Graph
test_l_graph	= L_graph ['a','b','c'] [(Bracket 1 Open),(Bracket 1 Close),(Bracket 2 Open),(Bracket 2 Close)] [(Vertice 1 Nothing),(Vertice 2 Nothing),(Vertice 3 Nothing)]
				[(LArc (Vertice 1 Nothing) (Just (Bracket 1 Open)) (Just 'a') (Vertice 1 Nothing)),
				(LArc (Vertice 1 Nothing) Nothing (Just 'a') (Vertice 2 Nothing)),
				(LArc (Vertice 2 Nothing) (Just (Bracket 1 Close)) (Just 'a') (Vertice 2 Nothing)),
				(LArc (Vertice 1 Nothing) (Just (Bracket 2 Open)) (Just 'b') (Vertice 2 Nothing)),
				(LArc (Vertice 2 Nothing) (Just (Bracket 2 Close)) (Just 'c') (Vertice 3 Nothing))]
				(Vertice 1 Nothing)
				[(Vertice 2 Nothing), (Vertice 3 Nothing)]


--
-- Ядро
-- Core [ Nest ]
-- VASF - вершины, переходы, начальная вершина, конечные вершины
--

data Core = Core [Nest] 
	deriving(Eq, Show)

data Nest = Fin Vertice | Reg Vertice (Maybe Bracket) (Maybe Char) (Nest)
	deriving(Eq, Show)



test_c11 :: Core
test_c11	= Core 
			[(Reg (Vertice 1 Nothing) Nothing (Just 'a') (Fin (Vertice 2 Nothing))),
			(Reg (Vertice 1 Nothing) (Just (Bracket 1 Open)) (Just 'a') (Reg (Vertice 1 Nothing) Nothing (Just 'a') (Reg (Vertice 2 Nothing) (Just (Bracket 1 Close)) (Just 'a') (Fin (Vertice 2 Nothing))))),
			(Reg (Vertice 1 Nothing) (Just (Bracket 2 Open)) (Just 'b') (Reg (Vertice 2 Nothing) (Just (Bracket 2 Close)) (Just 'c') (Fin (Vertice 3 Nothing))))]

test_c12 :: Core
test_c12	= Core
			[(Reg (Vertice 1 Nothing) Nothing (Just 'a') (Fin (Vertice 2 Nothing))),
			(Reg (Vertice 1 Nothing) (Just (Bracket 1 Open)) (Just 'a') (Reg (Vertice 1 Nothing) Nothing (Just 'a') (Reg (Vertice 2 Nothing) (Just (Bracket 1 Close)) (Just 'a') (Fin (Vertice 2 Nothing))))),
			(Reg (Vertice 1 Nothing) (Just (Bracket 2 Open)) (Just 'b') (Reg (Vertice 2 Nothing) (Just (Bracket 2 Close)) (Just 'c') (Fin (Vertice 3 Nothing)))),
			(Reg (Vertice 1 Nothing) (Just (Bracket 1 Open)) (Just 'a') (Reg (Vertice 1 Nothing) (Just (Bracket 1 Open)) (Just 'a') (Reg (Vertice 1 Nothing) Nothing (Just 'a') (Reg (Vertice 2 Nothing) (Just (Bracket 1 Close)) (Just 'a') (Reg (Vertice 2 Nothing) (Just (Bracket 1 Close)) (Just 'a') (Fin (Vertice 2 Nothing)))))))]

test_c12_notmixed :: Core
test_c12_notmixed	= Core
					[(Reg (Vertice 1 Nothing) Nothing (Just 'a') (Fin (Vertice 2 Nothing))),
					(Reg (Vertice 1 Nothing) (Just (Bracket 1 Open)) (Just 'a') (Reg (Vertice 1 (Just (Bracket 1 Open))) Nothing (Just 'a') (Reg (Vertice 2 (Just (Bracket 1 Open))) (Just (Bracket 1 Close)) (Just 'a') (Fin (Vertice 2 Nothing))))),
					(Reg (Vertice 1 Nothing) (Just (Bracket 2 Open)) (Just 'b') (Reg (Vertice 2 (Just (Bracket 2 Open))) (Just (Bracket 2 Close)) (Just 'c') (Fin (Vertice 3 Nothing)))),
					(Reg (Vertice 1 Nothing) (Just (Bracket 1 Open)) (Just 'a') (Reg (Vertice 1 (Just (Bracket 1 Open))) (Just (Bracket 1 Open)) (Just 'a') (Reg (Vertice 1 (Just (Bracket 1 Open))) Nothing (Just 'a') (Reg (Vertice 2 (Just (Bracket 1 Open))) (Just (Bracket 1 Close)) (Just 'a') (Reg (Vertice 2 (Just (Bracket 1 Open))) (Just (Bracket 1 Close)) (Just 'a') (Fin (Vertice 2 Nothing)))))))]



data VASF = VASF [Vertice] [Arc] Vertice [Vertice]
	deriving(Eq, Show)



-- Мне нужны:
-- функции создания ядер 1-1 и 1-2
-- функция перевода из L-графа со смешанными ядрами в L-граф без смешанных ядер
-- функция перевода из L-графа без смешанных ядер в НКА
-- фукнция проверки эквивалентности языков определенных L-графом и НКА ?


--core11 :: Graph -> Core
--core11 (NFA _ _ _ _ _)							= Core []
--core11 (L_graph _ _ vrts arcs strt fnsh)	= Core [Nest]


--core12 :: Graph -> Core
--core12 (NFA _ _ _ _ _)							= Core []
--core12 (L_graph _ _ vrts arcs strt fnsh)	= Core [Nest]


nestMember :: Vertice -> Nest -> Bool
nestMember v (Fin x)	= (v == x)
nestMember v (Reg x _ _ next)	| v == x	= True
								| otherwise	= nestMember v next


coreMember :: Vertice -> Core -> Bool
coreMember v (Core [])	= False
coreMember v (Core (x:xs))	| (nestMember v x)	= True
							| otherwise			= coreMember v (Core xs)



nestArc :: Arc -> Nest -> Bool
nestArc _ (Fin _) = False
nestArc a (Reg x brack chr (Fin y)) = (a == (LArc x brack chr y))
nestArc a (Reg x brack chr (Reg y nb nc next))	| (a == (LArc x brack chr y))	= True
												| otherwise						= nestArc a (Reg y nb nc next)


coreArc :: Arc -> Core -> Bool
coreArc a (Core []) = False
coreArc a (Core (v:vs))	| (nestArc a v)	= True
						| otherwise		= coreArc a (Core vs)



{-newVerts :: [Vertice] -> Core -> Core -> [Vertice]
newVerts [] _ _	= []
newVerts (x:xs) c11 c12	| not (coreMember x c11)	= x:(newVerts xs c11 c12)
						| otherwise					= -}

first :: VASF -> [Vertice]
first (VASF vertices _ _ _) = vertices

second :: VASF -> [Arc]
second (VASF _ arcs _ _) = arcs

third :: VASF -> Vertice
third (VASF _ _ start _) = start

fourth :: VASF -> [Vertice]
fourth (VASF _ _ _ finished) = finished



brackTyp :: Maybe Bracket -> Either String OpCl
brackTyp Nothing = Left "Don't need, don't care"
brackTyp (Just (Bracket _ typ)) = Right typ





empty_verts :: [Vertice] -> Core -> [Vertice]
empty_verts [] _ = []
empty_verts (x:xs) cr	| not (coreMember x cr)	= x:(empty_verts xs cr)
						| otherwise				= empty_verts xs cr


nest_verts :: Nest -> [Maybe Bracket] -> [Vertice]
nest_verts (Fin (Vertice name _)) _ = [(Vertice name Nothing)]
nest_verts (Reg (Vertice name _) nBrack _ nNest) (b:bs)	| (brackTyp nBrack) == Right Open	= (Vertice name b) : (nest_verts nNest (nBrack : (b:bs)))
														| (brackTyp nBrack) == Right Close	= (Vertice name b) : (nest_verts nNest bs)
														| otherwise							= (Vertice name b) : (nest_verts nNest (b:bs))


new_verts :: Core -> [Vertice]
new_verts (Core []) = []
new_verts (Core (v:cr)) = (nest_verts v [Nothing]) ++ (new_verts (Core cr))




empty_arcs :: [Arc] -> [Vertice] -> [Arc]
empty_arcs [] _ = []
empty_arcs ((LArc x b c y):as) v	| (elem x v) && (elem y v)	= (LArc x b c y):(empty_arcs as v)
									| otherwise					= empty_arcs as v


nest_arcs :: Nest -> [Maybe Bracket] -> [Arc]
nest_arcs (Fin _) _ = []
nest_arcs (Reg (Vertice name1 br1) nBrack nc (Fin (Vertice name2 br2))) (b:bs) = [(LArc (Vertice name1 b) nBrack nc (Vertice name2 Nothing))]
nest_arcs (Reg (Vertice name1 br1) nBrack nc (Reg (Vertice name2 br2) nb nc1 nn1)) (b:(b1:bs))	| (brackTyp nBrack) == Right Open	= (LArc (Vertice name1 b) nBrack nc (Vertice name2 nBrack)) : (nest_arcs (Reg (Vertice name2 br2) nb nc1 nn1) (nBrack : (b:(b1:bs))))
																								| (brackTyp nBrack) == Right Close	= (LArc (Vertice name1 b) nBrack nc (Vertice name2 b1)) : (nest_arcs (Reg (Vertice name2 br2) nb nc1 nn1) (b1:bs))
																								| otherwise							= (LArc (Vertice name1 b) nBrack nc (Vertice name2 b)) : (nest_arcs (Reg (Vertice name2 br2) nb nc1 nn1) (b:(b1:bs)))


new_arcs :: Core -> [Arc]
new_arcs (Core []) = []
new_arcs (Core (v:vs)) = (nest_arcs v [Nothing, Nothing]) ++ (new_arcs (Core vs))



{-starting_vert :: Vertice -> [Vertice] -> Vertice
starting_vert _ [] = Vertice 0 Nothing
starting_vert (Vertice s1 br) ((Vertice s2 Nothing):vs)	| s1 == s2	= Vertice s1 Nothing
														| otherwise	= starting_vert (Vertice s1 br) vs

finishing_vrts :: [Vertice] -> [Vertice] -> [Vertice]
finishing_vrts [] _ = []
finishing_vrts (f:vs) v_list = (starting_vert f v_list):(finishing_vrts vs v_list)-}



conn :: [Arc] -> [(Vertice,Vertice)] -> [Arc] -> [Arc]
conn _ [] _ = []
conn [] (vp:vps) arcs = conn arcs vps arcs
conn ((LArc (Vertice na1 Nothing) ac ab (Vertice na2 Nothing)):as) (((Vertice nv1 Nothing),(Vertice nv2 Nothing)):vps) arcs
			| (na1 == nv1) && (na2 == nv2)	= ((LArc (Vertice na1 Nothing) ac ab (Vertice na2 Nothing))):(conn arcs vps arcs)
			| otherwise						= conn as (((Vertice nv1 Nothing),(Vertice nv2 Nothing)):vps) arcs


noth_pairs :: [Vertice] -> [Vertice] -> [Vertice] -> [(Vertice, Vertice)]
noth_pairs [] _ _ = []
noth_pairs (v1:vs1) [] vs2 = noth_pairs vs1 vs2 vs2
noth_pairs ((Vertice n1 Nothing):vs1) ((Vertice n2 Nothing):vs2) vs3 = ((Vertice n1 Nothing),(Vertice n2 Nothing)):(noth_pairs ((Vertice n1 Nothing):vs1) vs2 vs3)


connect :: [Arc] -> [Vertice] -> [Vertice] -> [Arc]
connect as vs1 vs2 = conn as (noth_pairs vs1 vs2 vs2) as



change :: [Vertice] -> [Arc] -> Vertice -> [Vertice] -> Core -> Core -> VASF
change vs as st fi c11 c12	= VASF (removeDuplicates my_all_nvs) ( removeDuplicates (ras ++ nas ++ nc2as ++ rton) )
							st fi
							--( finishing_vrts fi my_all_nvs )--VASF vs as st fi
					where
						evs = (empty_verts vs c11)
						nvs = (new_verts c11)
						my_all_nvs = ( evs ++ nvs )
						ras = (empty_arcs as evs)
						nas = (new_arcs c11)
						nc2as = (new_arcs c12)
						rton = (connect as evs nvs)
						--c11 = core11 (L_graph [] [] vs as st fi)
						--c12 = core12 (L_graph [] [] vs as st fi)




mixed_notmixed :: Graph -> Core -> Core -> Graph
mixed_notmixed (NFA a b c d e) c11 c12	= NFA a b c d e
mixed_notmixed (L_graph alph br_al vrts arcs strt fnsh) c11 c12	= L_graph alph br_al (first vasf) (second vasf) (third vasf) (fourth vasf)
					where
--						l_g = L_graph alph br_al vrts arcs strt fnsh
						vasf = (change vrts arcs strt fnsh c11 c12)
--						c11 = (core11 l_g)
--						c12 = (core12 l_g)


loops :: Nest -> [Arc]
loops (Fin _) = []
loops (Reg v1 br ch (Fin v2)) | v1 == v2 = [(LArc v1 br ch v2)]
							  | otherwise = []
loops (Reg v1 br ch (Reg v2 br1 ch1 rest)) | v1 == v2 = (LArc v1 br ch v2):(loops (Reg v2 br1 ch1 rest))
										   | otherwise = (loops (Reg v2 br1 ch1 rest))

nl :: Nest -> Int -> ([Vertice], [Arc], Int)
nl nest	mn | empty loopn = ([], [], mn)
		   | (length loopn) == 1 = ([], (larc_to_nfaarc loopn), mn)
		   | (length loopn) == 2 = ([nv], [(NFAArc (fst_vtc (i_elem loopn 1)) (char (i_elem loopn 0)) nv),(NFAArc nv (char (i_elem loopn 1)) (snd_vtc (i_elem loopn 1)))], (mn + 1))
			where
				loopn = loops nest
				nv = Vertice mn Nothing


new_nfa_arcs_verts :: Core -> Int -> ([Vertice], [Arc], Int)
new_nfa_arcs_verts (Core []) mn = ([], [], mn)
new_nfa_arcs_verts (Core (nest:nr)) mn = ( (tuple_first nfa_loop) ++ (tuple_first next) , (tuple_second nfa_loop) ++ (tuple_second next) , (tuple_third next	) )
									where
										next = new_nfa_arcs_verts (Core nr) (tuple_third nfa_loop)
										nfa_loop = nl nest mn


larc_to_nfaarc :: [Arc] -> [Arc]
larc_to_nfaarc [] = []
larc_to_nfaarc ((NFAArc v1 ch v2):ar) = (NFAArc v1 ch v2):(larc_to_nfaarc ar)
larc_to_nfaarc ((LArc v1 _ ch v2):ar) = (NFAArc v1 ch v2):(larc_to_nfaarc ar)

delete_cycles :: [Arc] -> [Arc]
delete_cycles [] = []
delete_cycles ((LArc v1 br ch v2):ar) | v1 == v2 = (delete_cycles ar)
									  | otherwise = (LArc v1 br ch v2):(delete_cycles ar)

max_vtc_num :: [Vertice] -> Int
max_vtc_num [] = 0
max_vtc_num ((Vertice num _):vr) = max num (max_vtc_num vr)

notmixed_NFA :: Graph -> Core -> Graph
notmixed_NFA (NFA a b c d e) c12	= NFA a b c d e
notmixed_NFA (L_graph alph br_al vrts arcs strt fnsh) c12	= NFA alph (vrts ++ (tuple_first nfa_v_a)) ((larc_to_nfaarc (delete_cycles arcs)) ++ (tuple_second nfa_v_a)) strt fnsh
													where
														nfa_v_a = new_nfa_arcs_verts c12 ((max_vtc_num vrts) + 1)
														--c11 = core11 (L_graph alph br_al vrts arcs strt fnsh)
														--c12 = core12 (L_graph alph br_al vrts arcs strt fnsh)





--dfaVerts ::


match :: [(Vertice, [Vertice])] -> [Vertice] -> Vertice
match ((v, vl1):vr) vl2 | (vl1 == vl2) = v
						| otherwise = match vr vl2

my_all :: (a -> b) -> [a] -> [b]
my_all _ [] = []
my_all action (a:as) = (action a):(my_all action as)


v_a_arcs :: [Vertice] -> Char -> [Arc] -> [Arc]
v_a_arcs _ _ [] = []
v_a_arcs vrts a ((NFAArc v1 c v2):ar) | (v1 `elem` vrts) && (c == (Just a)) = (NFAArc v1 c v2):(v_a_arcs vrts a ar)
									  | otherwise = (v_a_arcs vrts a ar)


table_alph_evrts :: [Vertice] -> [Arc] -> [Char] -> [(Char, [Vertice])]
table_alph_evrts _ _ [] = []
table_alph_evrts vert_set arcs (a:as) = (a, (my_all (snd_vtc) (v_a_arcs vert_set a arcs))):(table_alph_evrts vert_set arcs as)


sev_next_vt :: [(Char, [Vertice])] -> [Arc] -> [Char] -> [([Vertice], [(Char, [Vertice])])]
sev_next_vt [] _ _ = []
sev_next_vt ((_, cur_vs):ts) arcs alphabet | (intersection vc vn) = vn
										   | otherwise = vc ++ vn
									where
										vc = (vertTable alphabet arcs cur_vs)
										vn = (sev_next_vt ts arcs alphabet)


vertTable :: [Char] -> [Arc] -> [Vertice] -> [([Vertice], [(Char, [Vertice])])]
vertTable alphabet arcs cur | (cur, tav) `elem` np = np
							| otherwise = (cur, tav):np
							where
								tav = table_alph_evrts cur arcs alphabet
								np = (sev_next_vt tav arcs alphabet)


vert_pairs :: [([Vertice], [(Char, [Vertice])])] -> Int -> [(Vertice, [Vertice])]
vert_pairs [] _ = []
vert_pairs (((v:vs), _):ls) mn | length vs == 0 = (v, (v:vs)):(vert_pairs ls mn)
							   | otherwise = ((Vertice mn Nothing), (v:vs)):(vert_pairs ls (mn + 1))


dfaArcs :: [([Vertice], [(Char, [Vertice])])] -> [(Vertice, [Vertice])] -> [Arc]
dfaArcs [] _ = []
dfaArcs ((_, []):lr) vp = (dfaArcs lr vp)
dfaArcs ((vl1, ((char, vl2):cr)):lr) vp = (NFAArc (match vp vl1) (Just char) (match vp vl2)):(dfaArcs ((vl1, cr):lr) vp)


intersection :: Eq a => [a] -> [a] -> Bool
intersection [] _ = False
intersection (a:as) ar | a `elem` ar = True
					   | otherwise = (intersection as ar)

dfaFinish :: [(Vertice, [Vertice])] -> [Vertice] -> [Vertice]
dfaFinish [] _ = []
dfaFinish ((v, vl):vr) fnsh | (intersection vl fnsh) = (v:(dfaFinish vr fnsh))
							| otherwise = (dfaFinish vr fnsh)


nfa_dfa :: Graph -> Graph
nfa_dfa (L_graph a b c d e f)	= L_graph a b c d e f
nfa_dfa (NFA alph vrts arcs strt fnsh)	= NFA alph (my_all (fst) nvp) (dfaArcs v_t nvp) strt (dfaFinish nvp fnsh)
										where
											v_t = vertTable alph arcs [strt]
											nvp = vert_pairs v_t ((max_vtc_num vrts) + 1)




