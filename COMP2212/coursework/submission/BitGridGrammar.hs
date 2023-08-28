{-# OPTIONS_GHC -w #-}
module BitGridGrammar where
import BitGridTokens

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (TileToken)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14

action_0 (34) = happyShift action_3
action_0 (35) = happyShift action_4
action_0 (36) = happyShift action_5
action_0 (37) = happyShift action_6
action_0 (4) = happyGoto action_7
action_0 (5) = happyGoto action_8
action_0 (14) = happyGoto action_9
action_0 _ = happyFail

action_1 (34) = happyShift action_3
action_1 (35) = happyShift action_4
action_1 (36) = happyShift action_5
action_1 (37) = happyShift action_6
action_1 (14) = happyGoto action_2
action_1 _ = happyFail

action_2 (58) = happyShift action_10
action_2 _ = happyFail

action_3 _ = happyReduce_97

action_4 _ = happyReduce_98

action_5 _ = happyReduce_99

action_6 _ = happyReduce_100

action_7 (64) = happyAccept
action_7 _ = happyFail

action_8 (34) = happyShift action_3
action_8 (35) = happyShift action_4
action_8 (36) = happyShift action_5
action_8 (37) = happyShift action_6
action_8 (14) = happyGoto action_12
action_8 _ = happyFail

action_9 (58) = happyShift action_10
action_9 (60) = happyShift action_11
action_9 _ = happyFail

action_10 (15) = happyShift action_16
action_10 _ = happyFail

action_11 (15) = happyShift action_15
action_11 _ = happyFail

action_12 (58) = happyShift action_13
action_12 (60) = happyShift action_14
action_12 _ = happyFail

action_13 (15) = happyShift action_47
action_13 _ = happyFail

action_14 (15) = happyShift action_46
action_14 _ = happyFail

action_15 (38) = happyShift action_23
action_15 (40) = happyShift action_24
action_15 (42) = happyShift action_25
action_15 (43) = happyShift action_26
action_15 (44) = happyShift action_27
action_15 (45) = happyShift action_28
action_15 (46) = happyShift action_29
action_15 (47) = happyShift action_30
action_15 (48) = happyShift action_31
action_15 (49) = happyShift action_32
action_15 (50) = happyShift action_33
action_15 (51) = happyShift action_34
action_15 (52) = happyShift action_35
action_15 (53) = happyShift action_36
action_15 (54) = happyShift action_37
action_15 (55) = happyShift action_38
action_15 (56) = happyShift action_39
action_15 (57) = happyShift action_40
action_15 (59) = happyShift action_41
action_15 (60) = happyShift action_42
action_15 (62) = happyShift action_43
action_15 (63) = happyShift action_44
action_15 (6) = happyGoto action_45
action_15 (7) = happyGoto action_18
action_15 (10) = happyGoto action_19
action_15 (11) = happyGoto action_20
action_15 (12) = happyGoto action_21
action_15 (13) = happyGoto action_22
action_15 _ = happyFail

action_16 (38) = happyShift action_23
action_16 (40) = happyShift action_24
action_16 (42) = happyShift action_25
action_16 (43) = happyShift action_26
action_16 (44) = happyShift action_27
action_16 (45) = happyShift action_28
action_16 (46) = happyShift action_29
action_16 (47) = happyShift action_30
action_16 (48) = happyShift action_31
action_16 (49) = happyShift action_32
action_16 (50) = happyShift action_33
action_16 (51) = happyShift action_34
action_16 (52) = happyShift action_35
action_16 (53) = happyShift action_36
action_16 (54) = happyShift action_37
action_16 (55) = happyShift action_38
action_16 (56) = happyShift action_39
action_16 (57) = happyShift action_40
action_16 (59) = happyShift action_41
action_16 (60) = happyShift action_42
action_16 (62) = happyShift action_43
action_16 (63) = happyShift action_44
action_16 (6) = happyGoto action_17
action_16 (7) = happyGoto action_18
action_16 (10) = happyGoto action_19
action_16 (11) = happyGoto action_20
action_16 (12) = happyGoto action_21
action_16 (13) = happyGoto action_22
action_16 _ = happyFail

action_17 (18) = happyShift action_106
action_17 _ = happyFail

action_18 _ = happyReduce_17

action_19 (20) = happyShift action_94
action_19 (21) = happyShift action_95
action_19 (22) = happyShift action_96
action_19 (23) = happyShift action_97
action_19 (24) = happyShift action_98
action_19 (25) = happyShift action_99
action_19 (26) = happyShift action_100
action_19 (27) = happyShift action_101
action_19 (28) = happyShift action_102
action_19 (29) = happyShift action_103
action_19 (32) = happyShift action_104
action_19 (33) = happyShift action_105
action_19 _ = happyReduce_5

action_20 (32) = happyShift action_92
action_20 (33) = happyShift action_93
action_20 _ = happyReduce_7

action_21 _ = happyReduce_8

action_22 (17) = happyShift action_81
action_22 (20) = happyShift action_82
action_22 (21) = happyShift action_83
action_22 (22) = happyShift action_84
action_22 (23) = happyShift action_85
action_22 (24) = happyShift action_86
action_22 (25) = happyShift action_87
action_22 (26) = happyShift action_88
action_22 (27) = happyShift action_89
action_22 (28) = happyShift action_90
action_22 (29) = happyShift action_91
action_22 _ = happyReduce_6

action_23 (38) = happyShift action_23
action_23 (40) = happyShift action_24
action_23 (42) = happyShift action_25
action_23 (43) = happyShift action_26
action_23 (44) = happyShift action_27
action_23 (45) = happyShift action_28
action_23 (46) = happyShift action_29
action_23 (47) = happyShift action_30
action_23 (48) = happyShift action_31
action_23 (49) = happyShift action_32
action_23 (50) = happyShift action_33
action_23 (51) = happyShift action_34
action_23 (52) = happyShift action_35
action_23 (53) = happyShift action_36
action_23 (54) = happyShift action_37
action_23 (55) = happyShift action_38
action_23 (56) = happyShift action_39
action_23 (57) = happyShift action_40
action_23 (59) = happyShift action_41
action_23 (60) = happyShift action_42
action_23 (62) = happyShift action_43
action_23 (63) = happyShift action_44
action_23 (7) = happyGoto action_18
action_23 (10) = happyGoto action_78
action_23 (11) = happyGoto action_79
action_23 (13) = happyGoto action_80
action_23 _ = happyFail

action_24 (40) = happyShift action_76
action_24 (59) = happyShift action_77
action_24 (9) = happyGoto action_75
action_24 _ = happyFail

action_25 (38) = happyShift action_59
action_25 (40) = happyShift action_24
action_25 (42) = happyShift action_25
action_25 (43) = happyShift action_26
action_25 (44) = happyShift action_27
action_25 (45) = happyShift action_28
action_25 (46) = happyShift action_29
action_25 (47) = happyShift action_30
action_25 (48) = happyShift action_31
action_25 (49) = happyShift action_32
action_25 (50) = happyShift action_33
action_25 (51) = happyShift action_34
action_25 (52) = happyShift action_35
action_25 (53) = happyShift action_36
action_25 (54) = happyShift action_37
action_25 (55) = happyShift action_38
action_25 (56) = happyShift action_39
action_25 (57) = happyShift action_40
action_25 (59) = happyShift action_41
action_25 (60) = happyShift action_42
action_25 (7) = happyGoto action_18
action_25 (10) = happyGoto action_72
action_25 (12) = happyGoto action_73
action_25 (13) = happyGoto action_74
action_25 _ = happyFail

action_26 (38) = happyShift action_59
action_26 (40) = happyShift action_24
action_26 (42) = happyShift action_25
action_26 (43) = happyShift action_26
action_26 (44) = happyShift action_27
action_26 (45) = happyShift action_28
action_26 (46) = happyShift action_29
action_26 (47) = happyShift action_30
action_26 (48) = happyShift action_31
action_26 (49) = happyShift action_32
action_26 (50) = happyShift action_33
action_26 (51) = happyShift action_34
action_26 (52) = happyShift action_35
action_26 (53) = happyShift action_36
action_26 (54) = happyShift action_37
action_26 (55) = happyShift action_38
action_26 (56) = happyShift action_39
action_26 (57) = happyShift action_40
action_26 (59) = happyShift action_41
action_26 (60) = happyShift action_42
action_26 (7) = happyGoto action_18
action_26 (10) = happyGoto action_69
action_26 (12) = happyGoto action_70
action_26 (13) = happyGoto action_71
action_26 _ = happyFail

action_27 (38) = happyShift action_54
action_27 (40) = happyShift action_24
action_27 (42) = happyShift action_25
action_27 (43) = happyShift action_26
action_27 (44) = happyShift action_27
action_27 (45) = happyShift action_28
action_27 (46) = happyShift action_29
action_27 (47) = happyShift action_30
action_27 (48) = happyShift action_31
action_27 (49) = happyShift action_32
action_27 (50) = happyShift action_33
action_27 (51) = happyShift action_34
action_27 (52) = happyShift action_35
action_27 (53) = happyShift action_36
action_27 (54) = happyShift action_37
action_27 (55) = happyShift action_38
action_27 (56) = happyShift action_39
action_27 (57) = happyShift action_40
action_27 (60) = happyShift action_42
action_27 (7) = happyGoto action_18
action_27 (10) = happyGoto action_68
action_27 _ = happyFail

action_28 (38) = happyShift action_54
action_28 (40) = happyShift action_24
action_28 (42) = happyShift action_25
action_28 (43) = happyShift action_26
action_28 (44) = happyShift action_27
action_28 (45) = happyShift action_28
action_28 (46) = happyShift action_29
action_28 (47) = happyShift action_30
action_28 (48) = happyShift action_31
action_28 (49) = happyShift action_32
action_28 (50) = happyShift action_33
action_28 (51) = happyShift action_34
action_28 (52) = happyShift action_35
action_28 (53) = happyShift action_36
action_28 (54) = happyShift action_37
action_28 (55) = happyShift action_38
action_28 (56) = happyShift action_39
action_28 (57) = happyShift action_40
action_28 (60) = happyShift action_42
action_28 (7) = happyGoto action_18
action_28 (10) = happyGoto action_67
action_28 _ = happyFail

action_29 (38) = happyShift action_54
action_29 (40) = happyShift action_24
action_29 (42) = happyShift action_25
action_29 (43) = happyShift action_26
action_29 (44) = happyShift action_27
action_29 (45) = happyShift action_28
action_29 (46) = happyShift action_29
action_29 (47) = happyShift action_30
action_29 (48) = happyShift action_31
action_29 (49) = happyShift action_32
action_29 (50) = happyShift action_33
action_29 (51) = happyShift action_34
action_29 (52) = happyShift action_35
action_29 (53) = happyShift action_36
action_29 (54) = happyShift action_37
action_29 (55) = happyShift action_38
action_29 (56) = happyShift action_39
action_29 (57) = happyShift action_40
action_29 (60) = happyShift action_42
action_29 (7) = happyGoto action_18
action_29 (10) = happyGoto action_66
action_29 _ = happyFail

action_30 (38) = happyShift action_54
action_30 (40) = happyShift action_24
action_30 (42) = happyShift action_25
action_30 (43) = happyShift action_26
action_30 (44) = happyShift action_27
action_30 (45) = happyShift action_28
action_30 (46) = happyShift action_29
action_30 (47) = happyShift action_30
action_30 (48) = happyShift action_31
action_30 (49) = happyShift action_32
action_30 (50) = happyShift action_33
action_30 (51) = happyShift action_34
action_30 (52) = happyShift action_35
action_30 (53) = happyShift action_36
action_30 (54) = happyShift action_37
action_30 (55) = happyShift action_38
action_30 (56) = happyShift action_39
action_30 (57) = happyShift action_40
action_30 (60) = happyShift action_42
action_30 (7) = happyGoto action_18
action_30 (10) = happyGoto action_65
action_30 _ = happyFail

action_31 (38) = happyShift action_54
action_31 (40) = happyShift action_24
action_31 (42) = happyShift action_25
action_31 (43) = happyShift action_26
action_31 (44) = happyShift action_27
action_31 (45) = happyShift action_28
action_31 (46) = happyShift action_29
action_31 (47) = happyShift action_30
action_31 (48) = happyShift action_31
action_31 (49) = happyShift action_32
action_31 (50) = happyShift action_33
action_31 (51) = happyShift action_34
action_31 (52) = happyShift action_35
action_31 (53) = happyShift action_36
action_31 (54) = happyShift action_37
action_31 (55) = happyShift action_38
action_31 (56) = happyShift action_39
action_31 (57) = happyShift action_40
action_31 (60) = happyShift action_42
action_31 (7) = happyGoto action_18
action_31 (10) = happyGoto action_64
action_31 _ = happyFail

action_32 (38) = happyShift action_59
action_32 (40) = happyShift action_24
action_32 (42) = happyShift action_25
action_32 (43) = happyShift action_26
action_32 (44) = happyShift action_27
action_32 (45) = happyShift action_28
action_32 (46) = happyShift action_29
action_32 (47) = happyShift action_30
action_32 (48) = happyShift action_31
action_32 (49) = happyShift action_32
action_32 (50) = happyShift action_33
action_32 (51) = happyShift action_34
action_32 (52) = happyShift action_35
action_32 (53) = happyShift action_36
action_32 (54) = happyShift action_37
action_32 (55) = happyShift action_38
action_32 (56) = happyShift action_39
action_32 (57) = happyShift action_40
action_32 (59) = happyShift action_41
action_32 (60) = happyShift action_42
action_32 (7) = happyGoto action_18
action_32 (10) = happyGoto action_62
action_32 (13) = happyGoto action_63
action_32 _ = happyFail

action_33 (38) = happyShift action_54
action_33 (40) = happyShift action_24
action_33 (42) = happyShift action_25
action_33 (43) = happyShift action_26
action_33 (44) = happyShift action_27
action_33 (45) = happyShift action_28
action_33 (46) = happyShift action_29
action_33 (47) = happyShift action_30
action_33 (48) = happyShift action_31
action_33 (49) = happyShift action_32
action_33 (50) = happyShift action_33
action_33 (51) = happyShift action_34
action_33 (52) = happyShift action_35
action_33 (53) = happyShift action_36
action_33 (54) = happyShift action_37
action_33 (55) = happyShift action_38
action_33 (56) = happyShift action_39
action_33 (57) = happyShift action_40
action_33 (60) = happyShift action_42
action_33 (7) = happyGoto action_18
action_33 (10) = happyGoto action_61
action_33 _ = happyFail

action_34 (38) = happyShift action_54
action_34 (40) = happyShift action_24
action_34 (42) = happyShift action_25
action_34 (43) = happyShift action_26
action_34 (44) = happyShift action_27
action_34 (45) = happyShift action_28
action_34 (46) = happyShift action_29
action_34 (47) = happyShift action_30
action_34 (48) = happyShift action_31
action_34 (49) = happyShift action_32
action_34 (50) = happyShift action_33
action_34 (51) = happyShift action_34
action_34 (52) = happyShift action_35
action_34 (53) = happyShift action_36
action_34 (54) = happyShift action_37
action_34 (55) = happyShift action_38
action_34 (56) = happyShift action_39
action_34 (57) = happyShift action_40
action_34 (60) = happyShift action_42
action_34 (7) = happyGoto action_18
action_34 (10) = happyGoto action_60
action_34 _ = happyFail

action_35 (38) = happyShift action_59
action_35 (40) = happyShift action_24
action_35 (42) = happyShift action_25
action_35 (43) = happyShift action_26
action_35 (44) = happyShift action_27
action_35 (45) = happyShift action_28
action_35 (46) = happyShift action_29
action_35 (47) = happyShift action_30
action_35 (48) = happyShift action_31
action_35 (49) = happyShift action_32
action_35 (50) = happyShift action_33
action_35 (51) = happyShift action_34
action_35 (52) = happyShift action_35
action_35 (53) = happyShift action_36
action_35 (54) = happyShift action_37
action_35 (55) = happyShift action_38
action_35 (56) = happyShift action_39
action_35 (57) = happyShift action_40
action_35 (59) = happyShift action_41
action_35 (60) = happyShift action_42
action_35 (7) = happyGoto action_18
action_35 (10) = happyGoto action_57
action_35 (13) = happyGoto action_58
action_35 _ = happyFail

action_36 (38) = happyShift action_54
action_36 (40) = happyShift action_24
action_36 (42) = happyShift action_25
action_36 (43) = happyShift action_26
action_36 (44) = happyShift action_27
action_36 (45) = happyShift action_28
action_36 (46) = happyShift action_29
action_36 (47) = happyShift action_30
action_36 (48) = happyShift action_31
action_36 (49) = happyShift action_32
action_36 (50) = happyShift action_33
action_36 (51) = happyShift action_34
action_36 (52) = happyShift action_35
action_36 (53) = happyShift action_36
action_36 (54) = happyShift action_37
action_36 (55) = happyShift action_38
action_36 (56) = happyShift action_39
action_36 (57) = happyShift action_40
action_36 (60) = happyShift action_42
action_36 (7) = happyGoto action_18
action_36 (10) = happyGoto action_56
action_36 _ = happyFail

action_37 (38) = happyShift action_54
action_37 (40) = happyShift action_24
action_37 (42) = happyShift action_25
action_37 (43) = happyShift action_26
action_37 (44) = happyShift action_27
action_37 (45) = happyShift action_28
action_37 (46) = happyShift action_29
action_37 (47) = happyShift action_30
action_37 (48) = happyShift action_31
action_37 (49) = happyShift action_32
action_37 (50) = happyShift action_33
action_37 (51) = happyShift action_34
action_37 (52) = happyShift action_35
action_37 (53) = happyShift action_36
action_37 (54) = happyShift action_37
action_37 (55) = happyShift action_38
action_37 (56) = happyShift action_39
action_37 (57) = happyShift action_40
action_37 (60) = happyShift action_42
action_37 (7) = happyGoto action_18
action_37 (10) = happyGoto action_55
action_37 _ = happyFail

action_38 (38) = happyShift action_54
action_38 (40) = happyShift action_24
action_38 (42) = happyShift action_25
action_38 (43) = happyShift action_26
action_38 (44) = happyShift action_27
action_38 (45) = happyShift action_28
action_38 (46) = happyShift action_29
action_38 (47) = happyShift action_30
action_38 (48) = happyShift action_31
action_38 (49) = happyShift action_32
action_38 (50) = happyShift action_33
action_38 (51) = happyShift action_34
action_38 (52) = happyShift action_35
action_38 (53) = happyShift action_36
action_38 (54) = happyShift action_37
action_38 (55) = happyShift action_38
action_38 (56) = happyShift action_39
action_38 (57) = happyShift action_40
action_38 (60) = happyShift action_42
action_38 (7) = happyGoto action_18
action_38 (10) = happyGoto action_53
action_38 _ = happyFail

action_39 (38) = happyShift action_23
action_39 (40) = happyShift action_24
action_39 (42) = happyShift action_25
action_39 (43) = happyShift action_26
action_39 (44) = happyShift action_27
action_39 (45) = happyShift action_28
action_39 (46) = happyShift action_29
action_39 (47) = happyShift action_30
action_39 (48) = happyShift action_31
action_39 (49) = happyShift action_32
action_39 (50) = happyShift action_33
action_39 (51) = happyShift action_34
action_39 (52) = happyShift action_35
action_39 (53) = happyShift action_36
action_39 (54) = happyShift action_37
action_39 (55) = happyShift action_38
action_39 (56) = happyShift action_39
action_39 (57) = happyShift action_40
action_39 (59) = happyShift action_41
action_39 (60) = happyShift action_42
action_39 (62) = happyShift action_43
action_39 (63) = happyShift action_44
action_39 (6) = happyGoto action_52
action_39 (7) = happyGoto action_18
action_39 (10) = happyGoto action_19
action_39 (11) = happyGoto action_20
action_39 (12) = happyGoto action_21
action_39 (13) = happyGoto action_22
action_39 _ = happyFail

action_40 (19) = happyShift action_51
action_40 _ = happyFail

action_41 _ = happyReduce_80

action_42 _ = happyReduce_16

action_43 _ = happyReduce_44

action_44 _ = happyReduce_45

action_45 (18) = happyShift action_50
action_45 _ = happyFail

action_46 (38) = happyShift action_23
action_46 (40) = happyShift action_24
action_46 (42) = happyShift action_25
action_46 (43) = happyShift action_26
action_46 (44) = happyShift action_27
action_46 (45) = happyShift action_28
action_46 (46) = happyShift action_29
action_46 (47) = happyShift action_30
action_46 (48) = happyShift action_31
action_46 (49) = happyShift action_32
action_46 (50) = happyShift action_33
action_46 (51) = happyShift action_34
action_46 (52) = happyShift action_35
action_46 (53) = happyShift action_36
action_46 (54) = happyShift action_37
action_46 (55) = happyShift action_38
action_46 (56) = happyShift action_39
action_46 (57) = happyShift action_40
action_46 (59) = happyShift action_41
action_46 (60) = happyShift action_42
action_46 (62) = happyShift action_43
action_46 (63) = happyShift action_44
action_46 (6) = happyGoto action_49
action_46 (7) = happyGoto action_18
action_46 (10) = happyGoto action_19
action_46 (11) = happyGoto action_20
action_46 (12) = happyGoto action_21
action_46 (13) = happyGoto action_22
action_46 _ = happyFail

action_47 (38) = happyShift action_23
action_47 (40) = happyShift action_24
action_47 (42) = happyShift action_25
action_47 (43) = happyShift action_26
action_47 (44) = happyShift action_27
action_47 (45) = happyShift action_28
action_47 (46) = happyShift action_29
action_47 (47) = happyShift action_30
action_47 (48) = happyShift action_31
action_47 (49) = happyShift action_32
action_47 (50) = happyShift action_33
action_47 (51) = happyShift action_34
action_47 (52) = happyShift action_35
action_47 (53) = happyShift action_36
action_47 (54) = happyShift action_37
action_47 (55) = happyShift action_38
action_47 (56) = happyShift action_39
action_47 (57) = happyShift action_40
action_47 (59) = happyShift action_41
action_47 (60) = happyShift action_42
action_47 (62) = happyShift action_43
action_47 (63) = happyShift action_44
action_47 (6) = happyGoto action_48
action_47 (7) = happyGoto action_18
action_47 (10) = happyGoto action_19
action_47 (11) = happyGoto action_20
action_47 (12) = happyGoto action_21
action_47 (13) = happyGoto action_22
action_47 _ = happyFail

action_48 (18) = happyShift action_184
action_48 _ = happyFail

action_49 (18) = happyShift action_183
action_49 _ = happyFail

action_50 _ = happyReduce_3

action_51 (61) = happyShift action_182
action_51 _ = happyFail

action_52 (38) = happyShift action_23
action_52 (40) = happyShift action_24
action_52 (42) = happyShift action_25
action_52 (43) = happyShift action_26
action_52 (44) = happyShift action_27
action_52 (45) = happyShift action_28
action_52 (46) = happyShift action_29
action_52 (47) = happyShift action_30
action_52 (48) = happyShift action_31
action_52 (49) = happyShift action_32
action_52 (50) = happyShift action_33
action_52 (51) = happyShift action_34
action_52 (52) = happyShift action_35
action_52 (53) = happyShift action_36
action_52 (54) = happyShift action_37
action_52 (55) = happyShift action_38
action_52 (56) = happyShift action_39
action_52 (57) = happyShift action_40
action_52 (59) = happyShift action_41
action_52 (60) = happyShift action_42
action_52 (62) = happyShift action_43
action_52 (63) = happyShift action_44
action_52 (6) = happyGoto action_181
action_52 (7) = happyGoto action_18
action_52 (10) = happyGoto action_19
action_52 (11) = happyGoto action_20
action_52 (12) = happyGoto action_21
action_52 (13) = happyGoto action_22
action_52 _ = happyFail

action_53 (38) = happyShift action_54
action_53 (40) = happyShift action_24
action_53 (42) = happyShift action_25
action_53 (43) = happyShift action_26
action_53 (44) = happyShift action_27
action_53 (45) = happyShift action_28
action_53 (46) = happyShift action_29
action_53 (47) = happyShift action_30
action_53 (48) = happyShift action_31
action_53 (49) = happyShift action_32
action_53 (50) = happyShift action_33
action_53 (51) = happyShift action_34
action_53 (52) = happyShift action_35
action_53 (53) = happyShift action_36
action_53 (54) = happyShift action_37
action_53 (55) = happyShift action_38
action_53 (56) = happyShift action_39
action_53 (57) = happyShift action_40
action_53 (60) = happyShift action_42
action_53 (7) = happyGoto action_18
action_53 (10) = happyGoto action_180
action_53 _ = happyFail

action_54 (38) = happyShift action_23
action_54 (40) = happyShift action_24
action_54 (42) = happyShift action_25
action_54 (43) = happyShift action_26
action_54 (44) = happyShift action_27
action_54 (45) = happyShift action_28
action_54 (46) = happyShift action_29
action_54 (47) = happyShift action_30
action_54 (48) = happyShift action_31
action_54 (49) = happyShift action_32
action_54 (50) = happyShift action_33
action_54 (51) = happyShift action_34
action_54 (52) = happyShift action_35
action_54 (53) = happyShift action_36
action_54 (54) = happyShift action_37
action_54 (55) = happyShift action_38
action_54 (56) = happyShift action_39
action_54 (57) = happyShift action_40
action_54 (59) = happyShift action_41
action_54 (60) = happyShift action_42
action_54 (62) = happyShift action_43
action_54 (63) = happyShift action_44
action_54 (7) = happyGoto action_18
action_54 (10) = happyGoto action_78
action_54 (11) = happyGoto action_174
action_54 (13) = happyGoto action_109
action_54 _ = happyFail

action_55 _ = happyReduce_37

action_56 (38) = happyShift action_54
action_56 (40) = happyShift action_24
action_56 (42) = happyShift action_25
action_56 (43) = happyShift action_26
action_56 (44) = happyShift action_27
action_56 (45) = happyShift action_28
action_56 (46) = happyShift action_29
action_56 (47) = happyShift action_30
action_56 (48) = happyShift action_31
action_56 (49) = happyShift action_32
action_56 (50) = happyShift action_33
action_56 (51) = happyShift action_34
action_56 (52) = happyShift action_35
action_56 (53) = happyShift action_36
action_56 (54) = happyShift action_37
action_56 (55) = happyShift action_38
action_56 (56) = happyShift action_39
action_56 (57) = happyShift action_40
action_56 (60) = happyShift action_42
action_56 (7) = happyGoto action_18
action_56 (10) = happyGoto action_179
action_56 _ = happyFail

action_57 (26) = happyShift action_100
action_57 (27) = happyShift action_101
action_57 (28) = happyShift action_102
action_57 (29) = happyShift action_103
action_57 (38) = happyShift action_59
action_57 (40) = happyShift action_24
action_57 (42) = happyShift action_25
action_57 (43) = happyShift action_26
action_57 (44) = happyShift action_27
action_57 (45) = happyShift action_28
action_57 (46) = happyShift action_29
action_57 (47) = happyShift action_30
action_57 (48) = happyShift action_31
action_57 (49) = happyShift action_32
action_57 (50) = happyShift action_33
action_57 (51) = happyShift action_34
action_57 (52) = happyShift action_35
action_57 (53) = happyShift action_36
action_57 (54) = happyShift action_37
action_57 (55) = happyShift action_38
action_57 (56) = happyShift action_39
action_57 (57) = happyShift action_40
action_57 (59) = happyShift action_41
action_57 (60) = happyShift action_42
action_57 (7) = happyGoto action_18
action_57 (10) = happyGoto action_177
action_57 (13) = happyGoto action_178
action_57 _ = happyFail

action_58 (26) = happyShift action_88
action_58 (27) = happyShift action_89
action_58 (28) = happyShift action_90
action_58 (29) = happyShift action_91
action_58 (38) = happyShift action_59
action_58 (40) = happyShift action_24
action_58 (42) = happyShift action_25
action_58 (43) = happyShift action_26
action_58 (44) = happyShift action_27
action_58 (45) = happyShift action_28
action_58 (46) = happyShift action_29
action_58 (47) = happyShift action_30
action_58 (48) = happyShift action_31
action_58 (49) = happyShift action_32
action_58 (50) = happyShift action_33
action_58 (51) = happyShift action_34
action_58 (52) = happyShift action_35
action_58 (53) = happyShift action_36
action_58 (54) = happyShift action_37
action_58 (55) = happyShift action_38
action_58 (56) = happyShift action_39
action_58 (57) = happyShift action_40
action_58 (59) = happyShift action_41
action_58 (60) = happyShift action_42
action_58 (7) = happyGoto action_18
action_58 (10) = happyGoto action_175
action_58 (13) = happyGoto action_176
action_58 _ = happyFail

action_59 (38) = happyShift action_23
action_59 (40) = happyShift action_24
action_59 (42) = happyShift action_25
action_59 (43) = happyShift action_26
action_59 (44) = happyShift action_27
action_59 (45) = happyShift action_28
action_59 (46) = happyShift action_29
action_59 (47) = happyShift action_30
action_59 (48) = happyShift action_31
action_59 (49) = happyShift action_32
action_59 (50) = happyShift action_33
action_59 (51) = happyShift action_34
action_59 (52) = happyShift action_35
action_59 (53) = happyShift action_36
action_59 (54) = happyShift action_37
action_59 (55) = happyShift action_38
action_59 (56) = happyShift action_39
action_59 (57) = happyShift action_40
action_59 (59) = happyShift action_41
action_59 (60) = happyShift action_42
action_59 (62) = happyShift action_43
action_59 (63) = happyShift action_44
action_59 (7) = happyGoto action_18
action_59 (10) = happyGoto action_78
action_59 (11) = happyGoto action_174
action_59 (13) = happyGoto action_80
action_59 _ = happyFail

action_60 _ = happyReduce_32

action_61 _ = happyReduce_31

action_62 (26) = happyShift action_100
action_62 (27) = happyShift action_101
action_62 (28) = happyShift action_102
action_62 (29) = happyShift action_103
action_62 (38) = happyShift action_54
action_62 (40) = happyShift action_24
action_62 (42) = happyShift action_25
action_62 (43) = happyShift action_26
action_62 (44) = happyShift action_27
action_62 (45) = happyShift action_28
action_62 (46) = happyShift action_29
action_62 (47) = happyShift action_30
action_62 (48) = happyShift action_31
action_62 (49) = happyShift action_32
action_62 (50) = happyShift action_33
action_62 (51) = happyShift action_34
action_62 (52) = happyShift action_35
action_62 (53) = happyShift action_36
action_62 (54) = happyShift action_37
action_62 (55) = happyShift action_38
action_62 (56) = happyShift action_39
action_62 (57) = happyShift action_40
action_62 (60) = happyShift action_42
action_62 (7) = happyGoto action_18
action_62 (10) = happyGoto action_173
action_62 _ = happyFail

action_63 (26) = happyShift action_88
action_63 (27) = happyShift action_89
action_63 (28) = happyShift action_90
action_63 (29) = happyShift action_91
action_63 (38) = happyShift action_54
action_63 (40) = happyShift action_24
action_63 (42) = happyShift action_25
action_63 (43) = happyShift action_26
action_63 (44) = happyShift action_27
action_63 (45) = happyShift action_28
action_63 (46) = happyShift action_29
action_63 (47) = happyShift action_30
action_63 (48) = happyShift action_31
action_63 (49) = happyShift action_32
action_63 (50) = happyShift action_33
action_63 (51) = happyShift action_34
action_63 (52) = happyShift action_35
action_63 (53) = happyShift action_36
action_63 (54) = happyShift action_37
action_63 (55) = happyShift action_38
action_63 (56) = happyShift action_39
action_63 (57) = happyShift action_40
action_63 (60) = happyShift action_42
action_63 (7) = happyGoto action_18
action_63 (10) = happyGoto action_172
action_63 _ = happyFail

action_64 _ = happyReduce_28

action_65 _ = happyReduce_27

action_66 _ = happyReduce_26

action_67 (38) = happyShift action_54
action_67 (40) = happyShift action_24
action_67 (42) = happyShift action_25
action_67 (43) = happyShift action_26
action_67 (44) = happyShift action_27
action_67 (45) = happyShift action_28
action_67 (46) = happyShift action_29
action_67 (47) = happyShift action_30
action_67 (48) = happyShift action_31
action_67 (49) = happyShift action_32
action_67 (50) = happyShift action_33
action_67 (51) = happyShift action_34
action_67 (52) = happyShift action_35
action_67 (53) = happyShift action_36
action_67 (54) = happyShift action_37
action_67 (55) = happyShift action_38
action_67 (56) = happyShift action_39
action_67 (57) = happyShift action_40
action_67 (60) = happyShift action_42
action_67 (7) = happyGoto action_18
action_67 (10) = happyGoto action_171
action_67 _ = happyFail

action_68 (38) = happyShift action_54
action_68 (40) = happyShift action_24
action_68 (42) = happyShift action_25
action_68 (43) = happyShift action_26
action_68 (44) = happyShift action_27
action_68 (45) = happyShift action_28
action_68 (46) = happyShift action_29
action_68 (47) = happyShift action_30
action_68 (48) = happyShift action_31
action_68 (49) = happyShift action_32
action_68 (50) = happyShift action_33
action_68 (51) = happyShift action_34
action_68 (52) = happyShift action_35
action_68 (53) = happyShift action_36
action_68 (54) = happyShift action_37
action_68 (55) = happyShift action_38
action_68 (56) = happyShift action_39
action_68 (57) = happyShift action_40
action_68 (60) = happyShift action_42
action_68 (7) = happyGoto action_18
action_68 (10) = happyGoto action_170
action_68 _ = happyFail

action_69 (26) = happyShift action_100
action_69 (27) = happyShift action_101
action_69 (28) = happyShift action_102
action_69 (29) = happyShift action_103
action_69 (38) = happyShift action_54
action_69 (40) = happyShift action_24
action_69 (42) = happyShift action_25
action_69 (43) = happyShift action_26
action_69 (44) = happyShift action_27
action_69 (45) = happyShift action_28
action_69 (46) = happyShift action_29
action_69 (47) = happyShift action_30
action_69 (48) = happyShift action_31
action_69 (49) = happyShift action_32
action_69 (50) = happyShift action_33
action_69 (51) = happyShift action_34
action_69 (52) = happyShift action_35
action_69 (53) = happyShift action_36
action_69 (54) = happyShift action_37
action_69 (55) = happyShift action_38
action_69 (56) = happyShift action_39
action_69 (57) = happyShift action_40
action_69 (60) = happyShift action_42
action_69 (7) = happyGoto action_18
action_69 (10) = happyGoto action_169
action_69 _ = happyFail

action_70 (38) = happyShift action_54
action_70 (40) = happyShift action_24
action_70 (42) = happyShift action_25
action_70 (43) = happyShift action_26
action_70 (44) = happyShift action_27
action_70 (45) = happyShift action_28
action_70 (46) = happyShift action_29
action_70 (47) = happyShift action_30
action_70 (48) = happyShift action_31
action_70 (49) = happyShift action_32
action_70 (50) = happyShift action_33
action_70 (51) = happyShift action_34
action_70 (52) = happyShift action_35
action_70 (53) = happyShift action_36
action_70 (54) = happyShift action_37
action_70 (55) = happyShift action_38
action_70 (56) = happyShift action_39
action_70 (57) = happyShift action_40
action_70 (60) = happyShift action_42
action_70 (7) = happyGoto action_18
action_70 (10) = happyGoto action_168
action_70 _ = happyFail

action_71 (17) = happyShift action_81
action_71 (26) = happyShift action_88
action_71 (27) = happyShift action_89
action_71 (28) = happyShift action_90
action_71 (29) = happyShift action_91
action_71 (38) = happyShift action_54
action_71 (40) = happyShift action_24
action_71 (42) = happyShift action_25
action_71 (43) = happyShift action_26
action_71 (44) = happyShift action_27
action_71 (45) = happyShift action_28
action_71 (46) = happyShift action_29
action_71 (47) = happyShift action_30
action_71 (48) = happyShift action_31
action_71 (49) = happyShift action_32
action_71 (50) = happyShift action_33
action_71 (51) = happyShift action_34
action_71 (52) = happyShift action_35
action_71 (53) = happyShift action_36
action_71 (54) = happyShift action_37
action_71 (55) = happyShift action_38
action_71 (56) = happyShift action_39
action_71 (57) = happyShift action_40
action_71 (60) = happyShift action_42
action_71 (7) = happyGoto action_18
action_71 (10) = happyGoto action_167
action_71 _ = happyFail

action_72 (26) = happyShift action_100
action_72 (27) = happyShift action_101
action_72 (28) = happyShift action_102
action_72 (29) = happyShift action_103
action_72 (38) = happyShift action_54
action_72 (40) = happyShift action_24
action_72 (42) = happyShift action_25
action_72 (43) = happyShift action_26
action_72 (44) = happyShift action_27
action_72 (45) = happyShift action_28
action_72 (46) = happyShift action_29
action_72 (47) = happyShift action_30
action_72 (48) = happyShift action_31
action_72 (49) = happyShift action_32
action_72 (50) = happyShift action_33
action_72 (51) = happyShift action_34
action_72 (52) = happyShift action_35
action_72 (53) = happyShift action_36
action_72 (54) = happyShift action_37
action_72 (55) = happyShift action_38
action_72 (56) = happyShift action_39
action_72 (57) = happyShift action_40
action_72 (60) = happyShift action_42
action_72 (7) = happyGoto action_18
action_72 (10) = happyGoto action_166
action_72 _ = happyFail

action_73 (38) = happyShift action_54
action_73 (40) = happyShift action_24
action_73 (42) = happyShift action_25
action_73 (43) = happyShift action_26
action_73 (44) = happyShift action_27
action_73 (45) = happyShift action_28
action_73 (46) = happyShift action_29
action_73 (47) = happyShift action_30
action_73 (48) = happyShift action_31
action_73 (49) = happyShift action_32
action_73 (50) = happyShift action_33
action_73 (51) = happyShift action_34
action_73 (52) = happyShift action_35
action_73 (53) = happyShift action_36
action_73 (54) = happyShift action_37
action_73 (55) = happyShift action_38
action_73 (56) = happyShift action_39
action_73 (57) = happyShift action_40
action_73 (60) = happyShift action_42
action_73 (7) = happyGoto action_18
action_73 (10) = happyGoto action_165
action_73 _ = happyFail

action_74 (17) = happyShift action_81
action_74 (26) = happyShift action_88
action_74 (27) = happyShift action_89
action_74 (28) = happyShift action_90
action_74 (29) = happyShift action_91
action_74 (38) = happyShift action_54
action_74 (40) = happyShift action_24
action_74 (42) = happyShift action_25
action_74 (43) = happyShift action_26
action_74 (44) = happyShift action_27
action_74 (45) = happyShift action_28
action_74 (46) = happyShift action_29
action_74 (47) = happyShift action_30
action_74 (48) = happyShift action_31
action_74 (49) = happyShift action_32
action_74 (50) = happyShift action_33
action_74 (51) = happyShift action_34
action_74 (52) = happyShift action_35
action_74 (53) = happyShift action_36
action_74 (54) = happyShift action_37
action_74 (55) = happyShift action_38
action_74 (56) = happyShift action_39
action_74 (57) = happyShift action_40
action_74 (60) = happyShift action_42
action_74 (7) = happyGoto action_18
action_74 (10) = happyGoto action_164
action_74 _ = happyFail

action_75 (41) = happyShift action_163
action_75 _ = happyFail

action_76 (59) = happyShift action_77
action_76 (9) = happyGoto action_162
action_76 _ = happyFail

action_77 (16) = happyShift action_161
action_77 _ = happyReduce_14

action_78 (20) = happyShift action_94
action_78 (21) = happyShift action_95
action_78 (22) = happyShift action_96
action_78 (23) = happyShift action_97
action_78 (24) = happyShift action_98
action_78 (25) = happyShift action_99
action_78 (26) = happyShift action_100
action_78 (27) = happyShift action_101
action_78 (28) = happyShift action_102
action_78 (29) = happyShift action_103
action_78 (32) = happyShift action_104
action_78 (33) = happyShift action_105
action_78 (39) = happyShift action_160
action_78 _ = happyFail

action_79 (32) = happyShift action_92
action_79 (33) = happyShift action_93
action_79 (39) = happyShift action_159
action_79 _ = happyFail

action_80 (20) = happyShift action_82
action_80 (21) = happyShift action_83
action_80 (22) = happyShift action_84
action_80 (23) = happyShift action_85
action_80 (24) = happyShift action_86
action_80 (25) = happyShift action_87
action_80 (26) = happyShift action_88
action_80 (27) = happyShift action_89
action_80 (28) = happyShift action_90
action_80 (29) = happyShift action_91
action_80 (39) = happyShift action_158
action_80 _ = happyFail

action_81 (38) = happyShift action_59
action_81 (40) = happyShift action_24
action_81 (42) = happyShift action_25
action_81 (43) = happyShift action_26
action_81 (44) = happyShift action_27
action_81 (45) = happyShift action_28
action_81 (46) = happyShift action_29
action_81 (47) = happyShift action_30
action_81 (48) = happyShift action_31
action_81 (49) = happyShift action_32
action_81 (50) = happyShift action_33
action_81 (51) = happyShift action_34
action_81 (52) = happyShift action_35
action_81 (53) = happyShift action_36
action_81 (54) = happyShift action_37
action_81 (55) = happyShift action_38
action_81 (56) = happyShift action_39
action_81 (57) = happyShift action_40
action_81 (59) = happyShift action_41
action_81 (60) = happyShift action_42
action_81 (7) = happyGoto action_18
action_81 (10) = happyGoto action_156
action_81 (13) = happyGoto action_157
action_81 _ = happyFail

action_82 (38) = happyShift action_59
action_82 (40) = happyShift action_24
action_82 (42) = happyShift action_25
action_82 (43) = happyShift action_26
action_82 (44) = happyShift action_27
action_82 (45) = happyShift action_28
action_82 (46) = happyShift action_29
action_82 (47) = happyShift action_30
action_82 (48) = happyShift action_31
action_82 (49) = happyShift action_32
action_82 (50) = happyShift action_33
action_82 (51) = happyShift action_34
action_82 (52) = happyShift action_35
action_82 (53) = happyShift action_36
action_82 (54) = happyShift action_37
action_82 (55) = happyShift action_38
action_82 (56) = happyShift action_39
action_82 (57) = happyShift action_40
action_82 (59) = happyShift action_41
action_82 (60) = happyShift action_42
action_82 (7) = happyGoto action_18
action_82 (10) = happyGoto action_154
action_82 (13) = happyGoto action_155
action_82 _ = happyFail

action_83 (38) = happyShift action_59
action_83 (40) = happyShift action_24
action_83 (42) = happyShift action_25
action_83 (43) = happyShift action_26
action_83 (44) = happyShift action_27
action_83 (45) = happyShift action_28
action_83 (46) = happyShift action_29
action_83 (47) = happyShift action_30
action_83 (48) = happyShift action_31
action_83 (49) = happyShift action_32
action_83 (50) = happyShift action_33
action_83 (51) = happyShift action_34
action_83 (52) = happyShift action_35
action_83 (53) = happyShift action_36
action_83 (54) = happyShift action_37
action_83 (55) = happyShift action_38
action_83 (56) = happyShift action_39
action_83 (57) = happyShift action_40
action_83 (59) = happyShift action_41
action_83 (60) = happyShift action_42
action_83 (7) = happyGoto action_18
action_83 (10) = happyGoto action_152
action_83 (13) = happyGoto action_153
action_83 _ = happyFail

action_84 (38) = happyShift action_59
action_84 (40) = happyShift action_24
action_84 (42) = happyShift action_25
action_84 (43) = happyShift action_26
action_84 (44) = happyShift action_27
action_84 (45) = happyShift action_28
action_84 (46) = happyShift action_29
action_84 (47) = happyShift action_30
action_84 (48) = happyShift action_31
action_84 (49) = happyShift action_32
action_84 (50) = happyShift action_33
action_84 (51) = happyShift action_34
action_84 (52) = happyShift action_35
action_84 (53) = happyShift action_36
action_84 (54) = happyShift action_37
action_84 (55) = happyShift action_38
action_84 (56) = happyShift action_39
action_84 (57) = happyShift action_40
action_84 (59) = happyShift action_41
action_84 (60) = happyShift action_42
action_84 (7) = happyGoto action_18
action_84 (10) = happyGoto action_150
action_84 (13) = happyGoto action_151
action_84 _ = happyFail

action_85 (38) = happyShift action_59
action_85 (40) = happyShift action_24
action_85 (42) = happyShift action_25
action_85 (43) = happyShift action_26
action_85 (44) = happyShift action_27
action_85 (45) = happyShift action_28
action_85 (46) = happyShift action_29
action_85 (47) = happyShift action_30
action_85 (48) = happyShift action_31
action_85 (49) = happyShift action_32
action_85 (50) = happyShift action_33
action_85 (51) = happyShift action_34
action_85 (52) = happyShift action_35
action_85 (53) = happyShift action_36
action_85 (54) = happyShift action_37
action_85 (55) = happyShift action_38
action_85 (56) = happyShift action_39
action_85 (57) = happyShift action_40
action_85 (59) = happyShift action_41
action_85 (60) = happyShift action_42
action_85 (7) = happyGoto action_18
action_85 (10) = happyGoto action_148
action_85 (13) = happyGoto action_149
action_85 _ = happyFail

action_86 (38) = happyShift action_59
action_86 (40) = happyShift action_24
action_86 (42) = happyShift action_25
action_86 (43) = happyShift action_26
action_86 (44) = happyShift action_27
action_86 (45) = happyShift action_28
action_86 (46) = happyShift action_29
action_86 (47) = happyShift action_30
action_86 (48) = happyShift action_31
action_86 (49) = happyShift action_32
action_86 (50) = happyShift action_33
action_86 (51) = happyShift action_34
action_86 (52) = happyShift action_35
action_86 (53) = happyShift action_36
action_86 (54) = happyShift action_37
action_86 (55) = happyShift action_38
action_86 (56) = happyShift action_39
action_86 (57) = happyShift action_40
action_86 (59) = happyShift action_41
action_86 (60) = happyShift action_42
action_86 (7) = happyGoto action_18
action_86 (10) = happyGoto action_146
action_86 (13) = happyGoto action_147
action_86 _ = happyFail

action_87 (38) = happyShift action_59
action_87 (40) = happyShift action_24
action_87 (42) = happyShift action_25
action_87 (43) = happyShift action_26
action_87 (44) = happyShift action_27
action_87 (45) = happyShift action_28
action_87 (46) = happyShift action_29
action_87 (47) = happyShift action_30
action_87 (48) = happyShift action_31
action_87 (49) = happyShift action_32
action_87 (50) = happyShift action_33
action_87 (51) = happyShift action_34
action_87 (52) = happyShift action_35
action_87 (53) = happyShift action_36
action_87 (54) = happyShift action_37
action_87 (55) = happyShift action_38
action_87 (56) = happyShift action_39
action_87 (57) = happyShift action_40
action_87 (59) = happyShift action_41
action_87 (60) = happyShift action_42
action_87 (7) = happyGoto action_18
action_87 (10) = happyGoto action_144
action_87 (13) = happyGoto action_145
action_87 _ = happyFail

action_88 (38) = happyShift action_59
action_88 (40) = happyShift action_24
action_88 (42) = happyShift action_25
action_88 (43) = happyShift action_26
action_88 (44) = happyShift action_27
action_88 (45) = happyShift action_28
action_88 (46) = happyShift action_29
action_88 (47) = happyShift action_30
action_88 (48) = happyShift action_31
action_88 (49) = happyShift action_32
action_88 (50) = happyShift action_33
action_88 (51) = happyShift action_34
action_88 (52) = happyShift action_35
action_88 (53) = happyShift action_36
action_88 (54) = happyShift action_37
action_88 (55) = happyShift action_38
action_88 (56) = happyShift action_39
action_88 (57) = happyShift action_40
action_88 (59) = happyShift action_41
action_88 (60) = happyShift action_42
action_88 (7) = happyGoto action_18
action_88 (10) = happyGoto action_142
action_88 (13) = happyGoto action_143
action_88 _ = happyFail

action_89 (38) = happyShift action_59
action_89 (40) = happyShift action_24
action_89 (42) = happyShift action_25
action_89 (43) = happyShift action_26
action_89 (44) = happyShift action_27
action_89 (45) = happyShift action_28
action_89 (46) = happyShift action_29
action_89 (47) = happyShift action_30
action_89 (48) = happyShift action_31
action_89 (49) = happyShift action_32
action_89 (50) = happyShift action_33
action_89 (51) = happyShift action_34
action_89 (52) = happyShift action_35
action_89 (53) = happyShift action_36
action_89 (54) = happyShift action_37
action_89 (55) = happyShift action_38
action_89 (56) = happyShift action_39
action_89 (57) = happyShift action_40
action_89 (59) = happyShift action_41
action_89 (60) = happyShift action_42
action_89 (7) = happyGoto action_18
action_89 (10) = happyGoto action_140
action_89 (13) = happyGoto action_141
action_89 _ = happyFail

action_90 (38) = happyShift action_59
action_90 (40) = happyShift action_24
action_90 (42) = happyShift action_25
action_90 (43) = happyShift action_26
action_90 (44) = happyShift action_27
action_90 (45) = happyShift action_28
action_90 (46) = happyShift action_29
action_90 (47) = happyShift action_30
action_90 (48) = happyShift action_31
action_90 (49) = happyShift action_32
action_90 (50) = happyShift action_33
action_90 (51) = happyShift action_34
action_90 (52) = happyShift action_35
action_90 (53) = happyShift action_36
action_90 (54) = happyShift action_37
action_90 (55) = happyShift action_38
action_90 (56) = happyShift action_39
action_90 (57) = happyShift action_40
action_90 (59) = happyShift action_41
action_90 (60) = happyShift action_42
action_90 (7) = happyGoto action_18
action_90 (10) = happyGoto action_138
action_90 (13) = happyGoto action_139
action_90 _ = happyFail

action_91 (38) = happyShift action_59
action_91 (40) = happyShift action_24
action_91 (42) = happyShift action_25
action_91 (43) = happyShift action_26
action_91 (44) = happyShift action_27
action_91 (45) = happyShift action_28
action_91 (46) = happyShift action_29
action_91 (47) = happyShift action_30
action_91 (48) = happyShift action_31
action_91 (49) = happyShift action_32
action_91 (50) = happyShift action_33
action_91 (51) = happyShift action_34
action_91 (52) = happyShift action_35
action_91 (53) = happyShift action_36
action_91 (54) = happyShift action_37
action_91 (55) = happyShift action_38
action_91 (56) = happyShift action_39
action_91 (57) = happyShift action_40
action_91 (59) = happyShift action_41
action_91 (60) = happyShift action_42
action_91 (7) = happyGoto action_18
action_91 (10) = happyGoto action_136
action_91 (13) = happyGoto action_137
action_91 _ = happyFail

action_92 (38) = happyShift action_23
action_92 (40) = happyShift action_24
action_92 (42) = happyShift action_25
action_92 (43) = happyShift action_26
action_92 (44) = happyShift action_27
action_92 (45) = happyShift action_28
action_92 (46) = happyShift action_29
action_92 (47) = happyShift action_30
action_92 (48) = happyShift action_31
action_92 (49) = happyShift action_32
action_92 (50) = happyShift action_33
action_92 (51) = happyShift action_34
action_92 (52) = happyShift action_35
action_92 (53) = happyShift action_36
action_92 (54) = happyShift action_37
action_92 (55) = happyShift action_38
action_92 (56) = happyShift action_39
action_92 (57) = happyShift action_40
action_92 (59) = happyShift action_41
action_92 (60) = happyShift action_42
action_92 (62) = happyShift action_43
action_92 (63) = happyShift action_44
action_92 (7) = happyGoto action_18
action_92 (10) = happyGoto action_134
action_92 (11) = happyGoto action_135
action_92 (13) = happyGoto action_109
action_92 _ = happyFail

action_93 (38) = happyShift action_23
action_93 (40) = happyShift action_24
action_93 (42) = happyShift action_25
action_93 (43) = happyShift action_26
action_93 (44) = happyShift action_27
action_93 (45) = happyShift action_28
action_93 (46) = happyShift action_29
action_93 (47) = happyShift action_30
action_93 (48) = happyShift action_31
action_93 (49) = happyShift action_32
action_93 (50) = happyShift action_33
action_93 (51) = happyShift action_34
action_93 (52) = happyShift action_35
action_93 (53) = happyShift action_36
action_93 (54) = happyShift action_37
action_93 (55) = happyShift action_38
action_93 (56) = happyShift action_39
action_93 (57) = happyShift action_40
action_93 (59) = happyShift action_41
action_93 (60) = happyShift action_42
action_93 (62) = happyShift action_43
action_93 (63) = happyShift action_44
action_93 (7) = happyGoto action_18
action_93 (10) = happyGoto action_132
action_93 (11) = happyGoto action_133
action_93 (13) = happyGoto action_109
action_93 _ = happyFail

action_94 (38) = happyShift action_59
action_94 (40) = happyShift action_24
action_94 (42) = happyShift action_25
action_94 (43) = happyShift action_26
action_94 (44) = happyShift action_27
action_94 (45) = happyShift action_28
action_94 (46) = happyShift action_29
action_94 (47) = happyShift action_30
action_94 (48) = happyShift action_31
action_94 (49) = happyShift action_32
action_94 (50) = happyShift action_33
action_94 (51) = happyShift action_34
action_94 (52) = happyShift action_35
action_94 (53) = happyShift action_36
action_94 (54) = happyShift action_37
action_94 (55) = happyShift action_38
action_94 (56) = happyShift action_39
action_94 (57) = happyShift action_40
action_94 (59) = happyShift action_41
action_94 (60) = happyShift action_42
action_94 (7) = happyGoto action_18
action_94 (10) = happyGoto action_130
action_94 (13) = happyGoto action_131
action_94 _ = happyFail

action_95 (38) = happyShift action_59
action_95 (40) = happyShift action_24
action_95 (42) = happyShift action_25
action_95 (43) = happyShift action_26
action_95 (44) = happyShift action_27
action_95 (45) = happyShift action_28
action_95 (46) = happyShift action_29
action_95 (47) = happyShift action_30
action_95 (48) = happyShift action_31
action_95 (49) = happyShift action_32
action_95 (50) = happyShift action_33
action_95 (51) = happyShift action_34
action_95 (52) = happyShift action_35
action_95 (53) = happyShift action_36
action_95 (54) = happyShift action_37
action_95 (55) = happyShift action_38
action_95 (56) = happyShift action_39
action_95 (57) = happyShift action_40
action_95 (59) = happyShift action_41
action_95 (60) = happyShift action_42
action_95 (7) = happyGoto action_18
action_95 (10) = happyGoto action_128
action_95 (13) = happyGoto action_129
action_95 _ = happyFail

action_96 (38) = happyShift action_59
action_96 (40) = happyShift action_24
action_96 (42) = happyShift action_25
action_96 (43) = happyShift action_26
action_96 (44) = happyShift action_27
action_96 (45) = happyShift action_28
action_96 (46) = happyShift action_29
action_96 (47) = happyShift action_30
action_96 (48) = happyShift action_31
action_96 (49) = happyShift action_32
action_96 (50) = happyShift action_33
action_96 (51) = happyShift action_34
action_96 (52) = happyShift action_35
action_96 (53) = happyShift action_36
action_96 (54) = happyShift action_37
action_96 (55) = happyShift action_38
action_96 (56) = happyShift action_39
action_96 (57) = happyShift action_40
action_96 (59) = happyShift action_41
action_96 (60) = happyShift action_42
action_96 (7) = happyGoto action_18
action_96 (10) = happyGoto action_126
action_96 (13) = happyGoto action_127
action_96 _ = happyFail

action_97 (38) = happyShift action_59
action_97 (40) = happyShift action_24
action_97 (42) = happyShift action_25
action_97 (43) = happyShift action_26
action_97 (44) = happyShift action_27
action_97 (45) = happyShift action_28
action_97 (46) = happyShift action_29
action_97 (47) = happyShift action_30
action_97 (48) = happyShift action_31
action_97 (49) = happyShift action_32
action_97 (50) = happyShift action_33
action_97 (51) = happyShift action_34
action_97 (52) = happyShift action_35
action_97 (53) = happyShift action_36
action_97 (54) = happyShift action_37
action_97 (55) = happyShift action_38
action_97 (56) = happyShift action_39
action_97 (57) = happyShift action_40
action_97 (59) = happyShift action_41
action_97 (60) = happyShift action_42
action_97 (7) = happyGoto action_18
action_97 (10) = happyGoto action_124
action_97 (13) = happyGoto action_125
action_97 _ = happyFail

action_98 (38) = happyShift action_59
action_98 (40) = happyShift action_24
action_98 (42) = happyShift action_25
action_98 (43) = happyShift action_26
action_98 (44) = happyShift action_27
action_98 (45) = happyShift action_28
action_98 (46) = happyShift action_29
action_98 (47) = happyShift action_30
action_98 (48) = happyShift action_31
action_98 (49) = happyShift action_32
action_98 (50) = happyShift action_33
action_98 (51) = happyShift action_34
action_98 (52) = happyShift action_35
action_98 (53) = happyShift action_36
action_98 (54) = happyShift action_37
action_98 (55) = happyShift action_38
action_98 (56) = happyShift action_39
action_98 (57) = happyShift action_40
action_98 (59) = happyShift action_41
action_98 (60) = happyShift action_42
action_98 (7) = happyGoto action_18
action_98 (10) = happyGoto action_122
action_98 (13) = happyGoto action_123
action_98 _ = happyFail

action_99 (38) = happyShift action_59
action_99 (40) = happyShift action_24
action_99 (42) = happyShift action_25
action_99 (43) = happyShift action_26
action_99 (44) = happyShift action_27
action_99 (45) = happyShift action_28
action_99 (46) = happyShift action_29
action_99 (47) = happyShift action_30
action_99 (48) = happyShift action_31
action_99 (49) = happyShift action_32
action_99 (50) = happyShift action_33
action_99 (51) = happyShift action_34
action_99 (52) = happyShift action_35
action_99 (53) = happyShift action_36
action_99 (54) = happyShift action_37
action_99 (55) = happyShift action_38
action_99 (56) = happyShift action_39
action_99 (57) = happyShift action_40
action_99 (59) = happyShift action_41
action_99 (60) = happyShift action_42
action_99 (7) = happyGoto action_18
action_99 (10) = happyGoto action_120
action_99 (13) = happyGoto action_121
action_99 _ = happyFail

action_100 (38) = happyShift action_59
action_100 (40) = happyShift action_24
action_100 (42) = happyShift action_25
action_100 (43) = happyShift action_26
action_100 (44) = happyShift action_27
action_100 (45) = happyShift action_28
action_100 (46) = happyShift action_29
action_100 (47) = happyShift action_30
action_100 (48) = happyShift action_31
action_100 (49) = happyShift action_32
action_100 (50) = happyShift action_33
action_100 (51) = happyShift action_34
action_100 (52) = happyShift action_35
action_100 (53) = happyShift action_36
action_100 (54) = happyShift action_37
action_100 (55) = happyShift action_38
action_100 (56) = happyShift action_39
action_100 (57) = happyShift action_40
action_100 (59) = happyShift action_41
action_100 (60) = happyShift action_42
action_100 (7) = happyGoto action_18
action_100 (10) = happyGoto action_118
action_100 (13) = happyGoto action_119
action_100 _ = happyFail

action_101 (38) = happyShift action_59
action_101 (40) = happyShift action_24
action_101 (42) = happyShift action_25
action_101 (43) = happyShift action_26
action_101 (44) = happyShift action_27
action_101 (45) = happyShift action_28
action_101 (46) = happyShift action_29
action_101 (47) = happyShift action_30
action_101 (48) = happyShift action_31
action_101 (49) = happyShift action_32
action_101 (50) = happyShift action_33
action_101 (51) = happyShift action_34
action_101 (52) = happyShift action_35
action_101 (53) = happyShift action_36
action_101 (54) = happyShift action_37
action_101 (55) = happyShift action_38
action_101 (56) = happyShift action_39
action_101 (57) = happyShift action_40
action_101 (59) = happyShift action_41
action_101 (60) = happyShift action_42
action_101 (7) = happyGoto action_18
action_101 (10) = happyGoto action_116
action_101 (13) = happyGoto action_117
action_101 _ = happyFail

action_102 (38) = happyShift action_59
action_102 (40) = happyShift action_24
action_102 (42) = happyShift action_25
action_102 (43) = happyShift action_26
action_102 (44) = happyShift action_27
action_102 (45) = happyShift action_28
action_102 (46) = happyShift action_29
action_102 (47) = happyShift action_30
action_102 (48) = happyShift action_31
action_102 (49) = happyShift action_32
action_102 (50) = happyShift action_33
action_102 (51) = happyShift action_34
action_102 (52) = happyShift action_35
action_102 (53) = happyShift action_36
action_102 (54) = happyShift action_37
action_102 (55) = happyShift action_38
action_102 (56) = happyShift action_39
action_102 (57) = happyShift action_40
action_102 (59) = happyShift action_41
action_102 (60) = happyShift action_42
action_102 (7) = happyGoto action_18
action_102 (10) = happyGoto action_114
action_102 (13) = happyGoto action_115
action_102 _ = happyFail

action_103 (38) = happyShift action_59
action_103 (40) = happyShift action_24
action_103 (42) = happyShift action_25
action_103 (43) = happyShift action_26
action_103 (44) = happyShift action_27
action_103 (45) = happyShift action_28
action_103 (46) = happyShift action_29
action_103 (47) = happyShift action_30
action_103 (48) = happyShift action_31
action_103 (49) = happyShift action_32
action_103 (50) = happyShift action_33
action_103 (51) = happyShift action_34
action_103 (52) = happyShift action_35
action_103 (53) = happyShift action_36
action_103 (54) = happyShift action_37
action_103 (55) = happyShift action_38
action_103 (56) = happyShift action_39
action_103 (57) = happyShift action_40
action_103 (59) = happyShift action_41
action_103 (60) = happyShift action_42
action_103 (7) = happyGoto action_18
action_103 (10) = happyGoto action_112
action_103 (13) = happyGoto action_113
action_103 _ = happyFail

action_104 (38) = happyShift action_23
action_104 (40) = happyShift action_24
action_104 (42) = happyShift action_25
action_104 (43) = happyShift action_26
action_104 (44) = happyShift action_27
action_104 (45) = happyShift action_28
action_104 (46) = happyShift action_29
action_104 (47) = happyShift action_30
action_104 (48) = happyShift action_31
action_104 (49) = happyShift action_32
action_104 (50) = happyShift action_33
action_104 (51) = happyShift action_34
action_104 (52) = happyShift action_35
action_104 (53) = happyShift action_36
action_104 (54) = happyShift action_37
action_104 (55) = happyShift action_38
action_104 (56) = happyShift action_39
action_104 (57) = happyShift action_40
action_104 (59) = happyShift action_41
action_104 (60) = happyShift action_42
action_104 (62) = happyShift action_43
action_104 (63) = happyShift action_44
action_104 (7) = happyGoto action_18
action_104 (10) = happyGoto action_110
action_104 (11) = happyGoto action_111
action_104 (13) = happyGoto action_109
action_104 _ = happyFail

action_105 (38) = happyShift action_23
action_105 (40) = happyShift action_24
action_105 (42) = happyShift action_25
action_105 (43) = happyShift action_26
action_105 (44) = happyShift action_27
action_105 (45) = happyShift action_28
action_105 (46) = happyShift action_29
action_105 (47) = happyShift action_30
action_105 (48) = happyShift action_31
action_105 (49) = happyShift action_32
action_105 (50) = happyShift action_33
action_105 (51) = happyShift action_34
action_105 (52) = happyShift action_35
action_105 (53) = happyShift action_36
action_105 (54) = happyShift action_37
action_105 (55) = happyShift action_38
action_105 (56) = happyShift action_39
action_105 (57) = happyShift action_40
action_105 (59) = happyShift action_41
action_105 (60) = happyShift action_42
action_105 (62) = happyShift action_43
action_105 (63) = happyShift action_44
action_105 (7) = happyGoto action_18
action_105 (10) = happyGoto action_107
action_105 (11) = happyGoto action_108
action_105 (13) = happyGoto action_109
action_105 _ = happyFail

action_106 _ = happyReduce_1

action_107 (20) = happyShift action_94
action_107 (21) = happyShift action_95
action_107 (22) = happyShift action_96
action_107 (23) = happyShift action_97
action_107 (24) = happyShift action_98
action_107 (25) = happyShift action_99
action_107 (26) = happyShift action_100
action_107 (27) = happyShift action_101
action_107 (28) = happyShift action_102
action_107 (29) = happyShift action_103
action_107 _ = happyReduce_53

action_108 _ = happyReduce_51

action_109 (20) = happyShift action_82
action_109 (21) = happyShift action_83
action_109 (22) = happyShift action_84
action_109 (23) = happyShift action_85
action_109 (24) = happyShift action_86
action_109 (25) = happyShift action_87
action_109 (26) = happyShift action_88
action_109 (27) = happyShift action_89
action_109 (28) = happyShift action_90
action_109 (29) = happyShift action_91
action_109 _ = happyFail

action_110 (20) = happyShift action_94
action_110 (21) = happyShift action_95
action_110 (22) = happyShift action_96
action_110 (23) = happyShift action_97
action_110 (24) = happyShift action_98
action_110 (25) = happyShift action_99
action_110 (26) = happyShift action_100
action_110 (27) = happyShift action_101
action_110 (28) = happyShift action_102
action_110 (29) = happyShift action_103
action_110 _ = happyReduce_52

action_111 _ = happyReduce_50

action_112 _ = happyReduce_88

action_113 _ = happyReduce_92

action_114 _ = happyReduce_87

action_115 _ = happyReduce_91

action_116 _ = happyReduce_86

action_117 _ = happyReduce_90

action_118 _ = happyReduce_85

action_119 _ = happyReduce_89

action_120 (26) = happyShift action_100
action_120 (27) = happyShift action_101
action_120 (28) = happyShift action_102
action_120 (29) = happyShift action_103
action_120 _ = happyReduce_65

action_121 (26) = happyShift action_88
action_121 (27) = happyShift action_89
action_121 (28) = happyShift action_90
action_121 (29) = happyShift action_91
action_121 _ = happyReduce_77

action_122 (26) = happyShift action_100
action_122 (27) = happyShift action_101
action_122 (28) = happyShift action_102
action_122 (29) = happyShift action_103
action_122 _ = happyReduce_64

action_123 (26) = happyShift action_88
action_123 (27) = happyShift action_89
action_123 (28) = happyShift action_90
action_123 (29) = happyShift action_91
action_123 _ = happyReduce_76

action_124 (26) = happyShift action_100
action_124 (27) = happyShift action_101
action_124 (28) = happyShift action_102
action_124 (29) = happyShift action_103
action_124 _ = happyReduce_61

action_125 (26) = happyShift action_88
action_125 (27) = happyShift action_89
action_125 (28) = happyShift action_90
action_125 (29) = happyShift action_91
action_125 _ = happyReduce_75

action_126 (26) = happyShift action_100
action_126 (27) = happyShift action_101
action_126 (28) = happyShift action_102
action_126 (29) = happyShift action_103
action_126 _ = happyReduce_60

action_127 (26) = happyShift action_88
action_127 (27) = happyShift action_89
action_127 (28) = happyShift action_90
action_127 (29) = happyShift action_91
action_127 _ = happyReduce_74

action_128 (26) = happyShift action_100
action_128 (27) = happyShift action_101
action_128 (28) = happyShift action_102
action_128 (29) = happyShift action_103
action_128 _ = happyReduce_63

action_129 (26) = happyShift action_88
action_129 (27) = happyShift action_89
action_129 (28) = happyShift action_90
action_129 (29) = happyShift action_91
action_129 _ = happyReduce_73

action_130 (26) = happyShift action_100
action_130 (27) = happyShift action_101
action_130 (28) = happyShift action_102
action_130 (29) = happyShift action_103
action_130 _ = happyReduce_62

action_131 (26) = happyShift action_88
action_131 (27) = happyShift action_89
action_131 (28) = happyShift action_90
action_131 (29) = happyShift action_91
action_131 _ = happyReduce_72

action_132 (20) = happyShift action_94
action_132 (21) = happyShift action_95
action_132 (22) = happyShift action_96
action_132 (23) = happyShift action_97
action_132 (24) = happyShift action_98
action_132 (25) = happyShift action_99
action_132 (26) = happyShift action_100
action_132 (27) = happyShift action_101
action_132 (28) = happyShift action_102
action_132 (29) = happyShift action_103
action_132 _ = happyReduce_49

action_133 _ = happyReduce_47

action_134 (20) = happyShift action_94
action_134 (21) = happyShift action_95
action_134 (22) = happyShift action_96
action_134 (23) = happyShift action_97
action_134 (24) = happyShift action_98
action_134 (25) = happyShift action_99
action_134 (26) = happyShift action_100
action_134 (27) = happyShift action_101
action_134 (28) = happyShift action_102
action_134 (29) = happyShift action_103
action_134 _ = happyReduce_48

action_135 _ = happyReduce_46

action_136 _ = happyReduce_96

action_137 _ = happyReduce_84

action_138 _ = happyReduce_95

action_139 _ = happyReduce_83

action_140 _ = happyReduce_94

action_141 _ = happyReduce_82

action_142 _ = happyReduce_93

action_143 _ = happyReduce_81

action_144 (26) = happyShift action_100
action_144 (27) = happyShift action_101
action_144 (28) = happyShift action_102
action_144 (29) = happyShift action_103
action_144 _ = happyReduce_71

action_145 (26) = happyShift action_88
action_145 (27) = happyShift action_89
action_145 (28) = happyShift action_90
action_145 (29) = happyShift action_91
action_145 _ = happyReduce_59

action_146 (26) = happyShift action_100
action_146 (27) = happyShift action_101
action_146 (28) = happyShift action_102
action_146 (29) = happyShift action_103
action_146 _ = happyReduce_70

action_147 (26) = happyShift action_88
action_147 (27) = happyShift action_89
action_147 (28) = happyShift action_90
action_147 (29) = happyShift action_91
action_147 _ = happyReduce_58

action_148 (26) = happyShift action_100
action_148 (27) = happyShift action_101
action_148 (28) = happyShift action_102
action_148 (29) = happyShift action_103
action_148 _ = happyReduce_67

action_149 (26) = happyShift action_88
action_149 (27) = happyShift action_89
action_149 (28) = happyShift action_90
action_149 (29) = happyShift action_91
action_149 _ = happyReduce_55

action_150 (26) = happyShift action_100
action_150 (27) = happyShift action_101
action_150 (28) = happyShift action_102
action_150 (29) = happyShift action_103
action_150 _ = happyReduce_66

action_151 (26) = happyShift action_88
action_151 (27) = happyShift action_89
action_151 (28) = happyShift action_90
action_151 (29) = happyShift action_91
action_151 _ = happyReduce_54

action_152 (26) = happyShift action_100
action_152 (27) = happyShift action_101
action_152 (28) = happyShift action_102
action_152 (29) = happyShift action_103
action_152 _ = happyReduce_69

action_153 (26) = happyShift action_88
action_153 (27) = happyShift action_89
action_153 (28) = happyShift action_90
action_153 (29) = happyShift action_91
action_153 _ = happyReduce_57

action_154 (26) = happyShift action_100
action_154 (27) = happyShift action_101
action_154 (28) = happyShift action_102
action_154 (29) = happyShift action_103
action_154 _ = happyReduce_68

action_155 (26) = happyShift action_88
action_155 (27) = happyShift action_89
action_155 (28) = happyShift action_90
action_155 (29) = happyShift action_91
action_155 _ = happyReduce_56

action_156 (26) = happyShift action_100
action_156 (27) = happyShift action_101
action_156 (28) = happyShift action_102
action_156 (29) = happyShift action_103
action_156 _ = happyFail

action_157 (26) = happyShift action_88
action_157 (27) = happyShift action_89
action_157 (28) = happyShift action_90
action_157 (29) = happyShift action_91
action_157 _ = happyReduce_78

action_158 _ = happyReduce_79

action_159 (30) = happyShift action_190
action_159 _ = happyReduce_43

action_160 _ = happyReduce_15

action_161 (59) = happyShift action_77
action_161 (9) = happyGoto action_189
action_161 _ = happyFail

action_162 (41) = happyShift action_188
action_162 _ = happyFail

action_163 _ = happyReduce_9

action_164 _ = happyReduce_20

action_165 _ = happyReduce_18

action_166 _ = happyReduce_22

action_167 _ = happyReduce_21

action_168 _ = happyReduce_19

action_169 _ = happyReduce_23

action_170 _ = happyReduce_24

action_171 _ = happyReduce_25

action_172 _ = happyReduce_29

action_173 _ = happyReduce_30

action_174 (32) = happyShift action_92
action_174 (33) = happyShift action_93
action_174 (39) = happyShift action_187
action_174 _ = happyFail

action_175 _ = happyReduce_34

action_176 _ = happyReduce_33

action_177 _ = happyReduce_36

action_178 _ = happyReduce_35

action_179 _ = happyReduce_38

action_180 _ = happyReduce_39

action_181 (38) = happyShift action_23
action_181 (40) = happyShift action_24
action_181 (42) = happyShift action_25
action_181 (43) = happyShift action_26
action_181 (44) = happyShift action_27
action_181 (45) = happyShift action_28
action_181 (46) = happyShift action_29
action_181 (47) = happyShift action_30
action_181 (48) = happyShift action_31
action_181 (49) = happyShift action_32
action_181 (50) = happyShift action_33
action_181 (51) = happyShift action_34
action_181 (52) = happyShift action_35
action_181 (53) = happyShift action_36
action_181 (54) = happyShift action_37
action_181 (55) = happyShift action_38
action_181 (56) = happyShift action_39
action_181 (57) = happyShift action_40
action_181 (59) = happyShift action_41
action_181 (60) = happyShift action_42
action_181 (62) = happyShift action_43
action_181 (63) = happyShift action_44
action_181 (6) = happyGoto action_186
action_181 (7) = happyGoto action_18
action_181 (10) = happyGoto action_19
action_181 (11) = happyGoto action_20
action_181 (12) = happyGoto action_21
action_181 (13) = happyGoto action_22
action_181 _ = happyFail

action_182 (19) = happyShift action_185
action_182 _ = happyFail

action_183 _ = happyReduce_4

action_184 _ = happyReduce_2

action_185 _ = happyReduce_41

action_186 (38) = happyShift action_54
action_186 (40) = happyShift action_24
action_186 (42) = happyShift action_25
action_186 (43) = happyShift action_26
action_186 (44) = happyShift action_27
action_186 (45) = happyShift action_28
action_186 (46) = happyShift action_29
action_186 (47) = happyShift action_30
action_186 (48) = happyShift action_31
action_186 (49) = happyShift action_32
action_186 (50) = happyShift action_33
action_186 (51) = happyShift action_34
action_186 (52) = happyShift action_35
action_186 (53) = happyShift action_36
action_186 (54) = happyShift action_37
action_186 (55) = happyShift action_38
action_186 (56) = happyShift action_39
action_186 (57) = happyShift action_40
action_186 (60) = happyShift action_42
action_186 (7) = happyGoto action_18
action_186 (10) = happyGoto action_194
action_186 _ = happyFail

action_187 (30) = happyShift action_190
action_187 _ = happyFail

action_188 (16) = happyShift action_193
action_188 (8) = happyGoto action_192
action_188 _ = happyFail

action_189 _ = happyReduce_13

action_190 (38) = happyShift action_191
action_190 _ = happyFail

action_191 (38) = happyShift action_23
action_191 (40) = happyShift action_24
action_191 (42) = happyShift action_25
action_191 (43) = happyShift action_26
action_191 (44) = happyShift action_27
action_191 (45) = happyShift action_28
action_191 (46) = happyShift action_29
action_191 (47) = happyShift action_30
action_191 (48) = happyShift action_31
action_191 (49) = happyShift action_32
action_191 (50) = happyShift action_33
action_191 (51) = happyShift action_34
action_191 (52) = happyShift action_35
action_191 (53) = happyShift action_36
action_191 (54) = happyShift action_37
action_191 (55) = happyShift action_38
action_191 (56) = happyShift action_39
action_191 (57) = happyShift action_40
action_191 (59) = happyShift action_41
action_191 (60) = happyShift action_42
action_191 (62) = happyShift action_43
action_191 (63) = happyShift action_44
action_191 (6) = happyGoto action_197
action_191 (7) = happyGoto action_18
action_191 (10) = happyGoto action_19
action_191 (11) = happyGoto action_20
action_191 (12) = happyGoto action_21
action_191 (13) = happyGoto action_22
action_191 _ = happyFail

action_192 (41) = happyShift action_196
action_192 _ = happyFail

action_193 (40) = happyShift action_195
action_193 _ = happyFail

action_194 _ = happyReduce_40

action_195 (59) = happyShift action_77
action_195 (9) = happyGoto action_199
action_195 _ = happyFail

action_196 _ = happyReduce_10

action_197 (39) = happyShift action_198
action_197 _ = happyFail

action_198 (31) = happyShift action_201
action_198 _ = happyFail

action_199 (41) = happyShift action_200
action_199 _ = happyFail

action_200 (16) = happyShift action_193
action_200 (8) = happyGoto action_203
action_200 _ = happyReduce_11

action_201 (38) = happyShift action_202
action_201 _ = happyFail

action_202 (38) = happyShift action_23
action_202 (40) = happyShift action_24
action_202 (42) = happyShift action_25
action_202 (43) = happyShift action_26
action_202 (44) = happyShift action_27
action_202 (45) = happyShift action_28
action_202 (46) = happyShift action_29
action_202 (47) = happyShift action_30
action_202 (48) = happyShift action_31
action_202 (49) = happyShift action_32
action_202 (50) = happyShift action_33
action_202 (51) = happyShift action_34
action_202 (52) = happyShift action_35
action_202 (53) = happyShift action_36
action_202 (54) = happyShift action_37
action_202 (55) = happyShift action_38
action_202 (56) = happyShift action_39
action_202 (57) = happyShift action_40
action_202 (59) = happyShift action_41
action_202 (60) = happyShift action_42
action_202 (62) = happyShift action_43
action_202 (63) = happyShift action_44
action_202 (6) = happyGoto action_204
action_202 (7) = happyGoto action_18
action_202 (10) = happyGoto action_19
action_202 (11) = happyGoto action_20
action_202 (12) = happyGoto action_21
action_202 (13) = happyGoto action_22
action_202 _ = happyFail

action_203 _ = happyReduce_12

action_204 (39) = happyShift action_205
action_204 _ = happyFail

action_205 _ = happyReduce_42

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (ProgramSingle happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 6 4 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (ProgramMultiple happy_var_1 happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 5 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_2)) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (AssignmentFinal happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 6 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_3)) `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (AssignmentMultiple happy_var_1 happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn6
		 (Operation happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn6
		 (Arith happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn6
		 (BooleanArith happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn6
		 (RangeArith happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (TileUnidimensional happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 6 7 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TileBidimensional happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 8 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileFinal happy_var_3
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 5 8 happyReduction_12
happyReduction_12 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileMultiple happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_3  9 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TokenInt _ happy_var_1))
	 =  HappyAbsSyn9
		 (IntComma happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyTerminal (TokenInt _ happy_var_1))
	 =  HappyAbsSyn9
		 (IntBinary happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn10
		 (Var happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn10
		 (Tile happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  10 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (HRepeatRange happy_var_2 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (VRepeatRange happy_var_2 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  10 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (HRepeat happy_var_2 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  10 happyReduction_21
happyReduction_21 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (VRepeat happy_var_2 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  10 happyReduction_22
happyReduction_22 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (HRepeatVar happy_var_2 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  10 happyReduction_23
happyReduction_23 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (VRepeatVar happy_var_2 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  10 happyReduction_24
happyReduction_24 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (HAdd happy_var_2 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  10 happyReduction_25
happyReduction_25 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (VAdd happy_var_2 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  10 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Rot90 happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  10 happyReduction_27
happyReduction_27 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Rot180 happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  10 happyReduction_28
happyReduction_28 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Rot270 happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  10 happyReduction_29
happyReduction_29 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Grow happy_var_2 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  10 happyReduction_30
happyReduction_30 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (GrowVar happy_var_2 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  10 happyReduction_31
happyReduction_31 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (HReflect happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  10 happyReduction_32
happyReduction_32 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (VReflect happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  10 happyReduction_33
happyReduction_33 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Blank happy_var_2 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  10 happyReduction_34
happyReduction_34 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (BlankArithVar happy_var_2 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  10 happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (BlankVarArith happy_var_2 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  10 happyReduction_36
happyReduction_36 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (BlankVar happy_var_2 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  10 happyReduction_37
happyReduction_37 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Not happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  10 happyReduction_38
happyReduction_38 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (And happy_var_2 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  10 happyReduction_39
happyReduction_39 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Or happy_var_2 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happyReduce 5 10 happyReduction_40
happyReduction_40 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Subtile happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 4 10 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyTerminal (TokenString _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Read happy_var_3
	) `HappyStk` happyRest

happyReduce_42 = happyReduce 11 10 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (ConditionThenElse happy_var_2 happy_var_6 happy_var_10
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_3  11 happyReduction_43
happyReduction_43 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  11 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn11
		 (BooleanTrue
	)

happyReduce_45 = happySpecReduce_1  11 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn11
		 (BooleanFalse
	)

happyReduce_46 = happySpecReduce_3  11 happyReduction_46
happyReduction_46 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanAnd happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  11 happyReduction_47
happyReduction_47 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanOr happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  11 happyReduction_48
happyReduction_48 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanAndBoolVar happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  11 happyReduction_49
happyReduction_49 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanOrBoolVar happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  11 happyReduction_50
happyReduction_50 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanAndVarBool happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  11 happyReduction_51
happyReduction_51 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanOrVarBool happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  11 happyReduction_52
happyReduction_52 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanAndVar happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  11 happyReduction_53
happyReduction_53 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanOrVar happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  11 happyReduction_54
happyReduction_54 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanLessThan happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  11 happyReduction_55
happyReduction_55 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanGreaterThan happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  11 happyReduction_56
happyReduction_56 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanLessThanEqual happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  11 happyReduction_57
happyReduction_57 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanGreaterThanEqual happy_var_1 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  11 happyReduction_58
happyReduction_58 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanEquals happy_var_1 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  11 happyReduction_59
happyReduction_59 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanNotEquals happy_var_1 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  11 happyReduction_60
happyReduction_60 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanLessThanVar happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  11 happyReduction_61
happyReduction_61 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanGreaterThanVar happy_var_1 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  11 happyReduction_62
happyReduction_62 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanLessThanEqualVar happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  11 happyReduction_63
happyReduction_63 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanGreaterThanEqualVar happy_var_1 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  11 happyReduction_64
happyReduction_64 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanEqualsVar happy_var_1 happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  11 happyReduction_65
happyReduction_65 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanNotEqualsVar happy_var_1 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  11 happyReduction_66
happyReduction_66 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanLessThanArithVar happy_var_1 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  11 happyReduction_67
happyReduction_67 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanGreaterThanArithVar happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  11 happyReduction_68
happyReduction_68 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanLessThanEqualArithVar happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  11 happyReduction_69
happyReduction_69 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanGreaterThanEqualArithVar happy_var_1 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  11 happyReduction_70
happyReduction_70 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanEqualsArithVar happy_var_1 happy_var_3
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  11 happyReduction_71
happyReduction_71 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanNotEqualsArithVar happy_var_1 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  11 happyReduction_72
happyReduction_72 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanLessThanEqualVarArith happy_var_1 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  11 happyReduction_73
happyReduction_73 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanGreaterThanEqualVarArith happy_var_1 happy_var_3
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  11 happyReduction_74
happyReduction_74 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanLessThanVarArith happy_var_1 happy_var_3
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  11 happyReduction_75
happyReduction_75 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanGreaterThanVarArith happy_var_1 happy_var_3
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_3  11 happyReduction_76
happyReduction_76 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanEqualsVarArith happy_var_1 happy_var_3
	)
happyReduction_76 _ _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  11 happyReduction_77
happyReduction_77 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (BooleanNotEqualsVarArith happy_var_1 happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  12 happyReduction_78
happyReduction_78 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (RangeDefined happy_var_1 happy_var_3
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  13 happyReduction_79
happyReduction_79 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  13 happyReduction_80
happyReduction_80 (HappyTerminal (TokenInt _ happy_var_1))
	 =  HappyAbsSyn13
		 (Int happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  13 happyReduction_81
happyReduction_81 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  13 happyReduction_82
happyReduction_82 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_3  13 happyReduction_83
happyReduction_83 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Multiply happy_var_1 happy_var_3
	)
happyReduction_83 _ _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  13 happyReduction_84
happyReduction_84 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Divide happy_var_1 happy_var_3
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_3  13 happyReduction_85
happyReduction_85 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn13
		 (PlusVar happy_var_1 happy_var_3
	)
happyReduction_85 _ _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  13 happyReduction_86
happyReduction_86 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn13
		 (MinusVar happy_var_1 happy_var_3
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_3  13 happyReduction_87
happyReduction_87 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn13
		 (MultiplyVar happy_var_1 happy_var_3
	)
happyReduction_87 _ _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_3  13 happyReduction_88
happyReduction_88 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn13
		 (DivideVar happy_var_1 happy_var_3
	)
happyReduction_88 _ _ _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_3  13 happyReduction_89
happyReduction_89 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn13
		 (PlusVarArith happy_var_1 happy_var_3
	)
happyReduction_89 _ _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_3  13 happyReduction_90
happyReduction_90 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn13
		 (MinusVarArith happy_var_1 happy_var_3
	)
happyReduction_90 _ _ _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  13 happyReduction_91
happyReduction_91 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn13
		 (MultiplyVarArith happy_var_1 happy_var_3
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_3  13 happyReduction_92
happyReduction_92 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn13
		 (DivideVarArith happy_var_1 happy_var_3
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  13 happyReduction_93
happyReduction_93 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (PlusArithVar happy_var_1 happy_var_3
	)
happyReduction_93 _ _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_3  13 happyReduction_94
happyReduction_94 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (MinusArithVar happy_var_1 happy_var_3
	)
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  13 happyReduction_95
happyReduction_95 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (MultiplyArithVar happy_var_1 happy_var_3
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_3  13 happyReduction_96
happyReduction_96 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (DivideArithVar happy_var_1 happy_var_3
	)
happyReduction_96 _ _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_1  14 happyReduction_97
happyReduction_97 _
	 =  HappyAbsSyn14
		 (TypeInt
	)

happyReduce_98 = happySpecReduce_1  14 happyReduction_98
happyReduction_98 _
	 =  HappyAbsSyn14
		 (TypeTile
	)

happyReduce_99 = happySpecReduce_1  14 happyReduction_99
happyReduction_99 _
	 =  HappyAbsSyn14
		 (TypeBool
	)

happyReduce_100 = happySpecReduce_1  14 happyReduction_100
happyReduction_100 _
	 =  HappyAbsSyn14
		 (TypeRange
	)

happyNewToken action sts stk [] =
	action 64 64 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenAssignment _ -> cont 15;
	TokenComma _ -> cont 16;
	TokenRange _ -> cont 17;
	TokenEndStmt _ -> cont 18;
	TokenQuotation _ -> cont 19;
	TokenLessThanEqual _ -> cont 20;
	TokenGreaterThanEqual _ -> cont 21;
	TokenLessThan _ -> cont 22;
	TokenGreaterThan _ -> cont 23;
	TokenEquals _ -> cont 24;
	TokenNotEquals _ -> cont 25;
	TokenPlus _ -> cont 26;
	TokenMinus _ -> cont 27;
	TokenMultiplication _ -> cont 28;
	TokenDivision _ -> cont 29;
	TokenThen _ -> cont 30;
	TokenElse _ -> cont 31;
	TokenBooleanAnd _ -> cont 32;
	TokenBooleanOr _ -> cont 33;
	TokenTypeInt _ -> cont 34;
	TokenTypeTile _ -> cont 35;
	TokenTypeBool _ -> cont 36;
	TokenTypeRange _ -> cont 37;
	TokenLParen _ -> cont 38;
	TokenRParen _ -> cont 39;
	TokenLBracket _ -> cont 40;
	TokenRBracket _ -> cont 41;
	TokenHRepeat _ -> cont 42;
	TokenVRepeat _ -> cont 43;
	TokenHAdd _ -> cont 44;
	TokenVAdd _ -> cont 45;
	TokenRot90 _ -> cont 46;
	TokenRot180 _ -> cont 47;
	TokenRot270 _ -> cont 48;
	TokenGrow _ -> cont 49;
	TokenHReflect _ -> cont 50;
	TokenVReflect _ -> cont 51;
	TokenBlank _ -> cont 52;
	TokenAnd _ -> cont 53;
	TokenNot _ -> cont 54;
	TokenOr _ -> cont 55;
	TokenSubtile _ -> cont 56;
	TokenRead _ -> cont 57;
	TokenFinal _ -> cont 58;
	TokenInt _ happy_dollar_dollar -> cont 59;
	TokenVar _ happy_dollar_dollar -> cont 60;
	TokenString _ happy_dollar_dollar -> cont 61;
	TokenTrue _ -> cont 62;
	TokenFalse _ -> cont 63;
	_ -> happyError' (tk:tks)
	}

happyError_ 64 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(TileToken)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseCalc tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [TileToken] -> a
parseError [] = error "Unidentified Parse Error"
parseError xs = error ("Parse error at " ++ show (tokenPosn (xs !! 0)))

data Program = ProgramSingle Type Vars
             | ProgramMultiple Assignments Type Vars
             deriving Show

data Assignments = AssignmentFinal Type String Vars
                 | AssignmentMultiple Assignments Type String Vars
                 deriving Show

data Vars = Operation Operation
          | Arith Arith
          | BooleanArith BooleanArith
          | RangeArith RangeArith
           deriving Show

data Arr = TileUnidimensional Binaries
         | TileBidimensional Binaries Arrs
         deriving Show

data Arrs = TileFinal Binaries
          | TileMultiple Binaries Arrs
          deriving Show

data Binaries = IntBinary Int
              | IntComma Int Binaries
              deriving Show

data Operation = Var String
               | Tile Arr
               | HRepeatRange RangeArith Operation
               | VRepeatRange RangeArith Operation
               | HRepeat Arith Operation
               | VRepeat Arith Operation
               | HRepeatVar Operation Operation
               | VRepeatVar Operation Operation
               | HAdd Operation Operation
               | VAdd Operation Operation
               | Rot90 Operation
               | Rot180 Operation
               | Rot270 Operation
               | Grow Arith Operation
               | GrowVar Operation Operation
               | HReflect Operation
               | VReflect Operation
               | Blank Arith Arith
               | BlankArithVar Arith Operation
               | BlankVarArith Operation Arith
               | BlankVar Operation Operation
               | Not Operation
               | And Operation Operation
               | Or Operation Operation
               | ConditionThenElse BooleanArith Vars Vars
               | Subtile Vars Vars Vars Operation
               | Read String
               deriving Show

data BooleanArith = BooleanAnd BooleanArith BooleanArith
                  | BooleanOr BooleanArith BooleanArith
                  | BooleanAndVar Operation Operation
                  | BooleanOrVar Operation Operation
                  | BooleanAndVarBool Operation BooleanArith
                  | BooleanOrVarBool Operation BooleanArith
                  | BooleanAndBoolVar BooleanArith Operation
                  | BooleanOrBoolVar BooleanArith Operation

                  | BooleanLessThan Arith Arith
                  | BooleanGreaterThan Arith Arith
                  | BooleanLessThanEqual Arith Arith
                  | BooleanGreaterThanEqual Arith Arith
                  | BooleanEquals Arith Arith
                  | BooleanNotEquals Arith Arith

                  | BooleanLessThanVar Operation Operation
                  | BooleanGreaterThanVar Operation Operation
                  | BooleanLessThanEqualVar Operation Operation
                  | BooleanGreaterThanEqualVar Operation Operation
                  | BooleanEqualsVar Operation Operation
                  | BooleanNotEqualsVar Operation Operation

                  | BooleanAndBoolArithVar BooleanArith Operation
                  | BooleanOrBoolArithVar BooleanArith Operation
                  | BooleanLessThanArithVar Arith Operation
                  | BooleanGreaterThanArithVar Arith Operation
                  | BooleanLessThanEqualArithVar Arith Operation
                  | BooleanGreaterThanEqualArithVar Arith Operation
                  | BooleanEqualsArithVar Arith Operation
                  | BooleanNotEqualsArithVar Arith Operation

                  | BooleanAndVarBoolArith Operation BooleanArith
                  | BooleanOrVarBoolArith Operation BooleanArith
                  | BooleanLessThanVarArith Operation Arith
                  | BooleanGreaterThanVarArith Operation Arith
                  | BooleanLessThanEqualVarArith Operation Arith
                  | BooleanGreaterThanEqualVarArith Operation Arith
                  | BooleanEqualsVarArith Operation Arith
                  | BooleanNotEqualsVarArith Operation Arith

                  | BooleanTrue
                  | BooleanFalse
                  deriving Show

data Arith = Plus Arith Arith
           | Minus Arith Arith
           | Multiply Arith Arith
           | Divide Arith Arith
           | PlusVar Operation Operation
           | MinusVar Operation Operation
           | MultiplyVar Operation Operation
           | DivideVar Operation Operation
           | PlusVarArith Operation Arith
           | MinusVarArith Operation Arith
           | MultiplyVarArith Operation Arith
           | DivideVarArith Operation Arith
           | PlusArithVar Arith Operation
           | MinusArithVar Arith Operation
           | MultiplyArithVar Arith Operation
           | DivideArithVar Arith Operation
           | Int Int
           deriving Show

data Type = TypeInt | TypeTile | TypeBool | TypeRange deriving (Show,Eq)

data RangeArith = RangeDefined Arith Arith deriving Show
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4























# 5 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
