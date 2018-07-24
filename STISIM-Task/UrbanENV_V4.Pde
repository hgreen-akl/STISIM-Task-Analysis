METRIC
//
//
//    Roadway with maximum quick changing curves
//
//
//
//
//    Define some texture map parameters
//
//

{Tags
ForeSlope = C:\STISIM3\Data\Textures\Grass06.Jpg
Lanes = C:\STISIM3\Data\Textures\Road19.Jpg
Shoulder = C:\STISIM3\Data\Textures\Road18.Jpg
SpeedLimit = C:\STISIM3\Data\EuroSigns\Speed Limit\Speed_Limit_50.Mka 
Sound = C:\STISIM3\Sound
CarBlock3 = C:\STISIM3\Data\Vehicles\Car_Block_3.mka
CarBlock4 = C:\STISIM3\Data\Vehicles\Car_Block_4.mka
CarBlock5 = C:\STISIM3\Data\Vehicles\Car_Block_5.mka
Streetlight = C:\STISIM3\Data\Miscellaneous\StreetLight_1.Mka
Staticobj = C:\STISIM3\Data\Buildings
Housegroup = C:\STISIM3\Data\Buildings\House\House_Group.Mka
Bench = C:\STISIM3\Data\Miscellaneous\Bench.mesh
}

//
//
//    Define the roadway characterisitics
//
//
// ROAD: Double Lane; ---- narrower than country, with footpath and no hills - Last line dfines the centre median parameters 
    0, ROADWAY, 7, 4, 2, 2, 0.4, 3.5, 7, 0.3, 0.2, 100, &
      0,      %Lanes%,   0,3.8,12, &
      0,      %Lanes%,   0,3.8,12, &
      0,  6,  %Shoulder%,  0,24,24, &
      0,  6,  %Shoulder%,  0,24,24, &
      0,  80, %Foreslope%,  0,24,24, &
      0,  80, %Foreslope%,  0,24,24, &
              %Foreslope%,  0,24,24, &
              %Foreslope%,  0,24,24, ,,,, 

// -----------------------------------------------  SPEED LIMITS

0, Speed Limit, 54
0, Sign, 100, 100, %SpeedLimit%,  0, 0, 0
1500, Sign, 100, 100, %SpeedLimit%,  0, 0, 0
3000, Sign, 100, 100, %SpeedLimit%,  0, 0, 0
4500, Sign, 100, 100, %SpeedLimit%,  0, 0, 0
6000, Sign, 100, 100, %SpeedLimit%,  0, 0, 0
7500, Sign, 100, 100, %SpeedLimit%,  0, 0, 0
9000, Sign, 100, 100, %SpeedLimit%,  0, 0, 0

//
//
// Add Traffic lights to all the intersections
//

 500, Signal Light, -250, 0, 0, 20, 2, -1.5, 6, 1, 0, 0, 0, 1
 1000, Signal Light, -250, 100{5}, 3, 10, 0, -1.5, 6, 1, 0, 0, 0, 1
 1700, Signal Light, -250, 0, 0, 4, 2, -1.5, 6, 2, 0, 0, 0, 1
 2200, Signal Light, -250, 0, 0, 22, 2, -1.5, 6, 2, 0, 0, 0, 1
 2900, Signal Light, -250, 0, 0, 0, 0, -1.5, 6, 2, 0, 0, 0, 1
 3600, Signal Light, -250, 0, 0, 13, 2, -1.5, 6, 2, 0, 0, 0, 1
 4300, Signal Light, -250, 0, 0, 0, 0, -1.5, 6, 2, 0, 0, 0, 1
 5700, Signal Light, -250, 0, 0, 15, 2, -1.5, 6, 2, 0, 0, 0, 1
 6800, Signal Light, -250, 0, 0, 0, 2, -1.5, 6, 2, 0, 0, 0, 1 
 7400, Signal Light, -250, 0, 0, 20, 2, -1.5, 6, 2, 0, 0, 0, 1
 8100, Signal Light, -250, 0, 0, 15, 2, -1.5, 6, 2, 0, 0, 0, 1
 8700, Signal Light, -250, 0, 0, 0, 2, -1.5, 6, 2, 0, 0, 0, 1
 
//
//    Add in Intersections 
//  Distance, Intersection, type, appear distance = 0 , turn permissions = 1, crosswalk type, crosswalk type, turn compliance, auditory warninng filepath, 
// A total of 15 intersections with 5 of each desired turn
// Randomised order of turns : Right, Straight, Right, Left, Right, Straight, Left, Right, Left, Straight, Left, Straight, Right, Left, Straight
//

 500.0	,	Intersection, 0, 250, 1, 2, 2, 1
 1000.0	,	Intersection, 0, 250, 1, 2, 2, 0
 1700.0	,	Intersection, 0, 250, 1, 2, 2, 1
 2200.0	,	Intersection, 0, 250, 1, 2, 2, 2
 2900.0	,	Intersection, 0, 250, 1, 2, 2, 1
 3600.0	,	Intersection, 0, 250, 1, 2, 2, 0
 4300.0	,	Intersection, 0, 250, 1, 2, 2, 2
 5700.0	,	Intersection, 0, 250, 1, 2, 2, 1
 6800.0	,	Intersection, 0, 250, 1, 2, 2, 2
 7400.0	,	Intersection, 0, 250, 1, 2, 2, 0
 8100.0	,	Intersection, 0, 250, 1, 2, 2, 2
 8700.0	,	Intersection, 0, 250, 1, 2, 2, 0

//
//  ADD Audio Queues to signal the correct way through the intersection
//
 
 400 , Play Recording, %Sound%\NextRight.Wav, 0, 100
 1600, Play Recording, %Sound%\NextRight.Wav, 0, 100
 2100, Play Recording, %Sound%\NextLeft.Wav, 0, 100
 2800, Play Recording, %Sound%\NextRight.Wav, 0, 100
 4200, Play Recording, %Sound%\NextLeft.Wav, 0, 100
 5600, Play Recording, %Sound%\NextRight.Wav, 0, 100
 6700, Play Recording, %Sound%\NextLeft.Wav, 0, 100
 8000, Play Recording, %Sound%\NextLeft.Wav, 0, 100
  

  
//  
// ADD in two construction zones
//
// Blocking Left lane

 2600, Barrel, 260, -11.5{0}, 5, 0
 2600, Barrel, 265, -11{0}, 5, 0
 2600, Barrel, 270, -10{0}, 5, 0
 2600, Barrel, 280, -9{0}, 5, 0
 2600, Barrel, 292, -8{0}, 5, 0
 2600, Barrel, 305, -7{0}, 5, 0
 2600, Barrel, 325, -7{0}, 5, 0
 2600, Barrel, 345, -7{0}, 5, 0
 2600, Barrel, 365, -7{0}, 5, 0
 2600, Barrel, 385, -7{0}, 5, 0
 2600, Barrel, 405, -7{0}, 5, 0
 2600, Barrel, 425, -8{0}, 5, 0
 2600, Barrel, 435, -10{0}, 5, 0
 2600, Barrel, 440, -11{0}, 5, 0
 2600, Vehicles, 320, -9.5{0}, 0{0}, 1, CN4, 1, 0
 2600, Vehicles, 350, -9{0}, 0{0}, 1, CN1, 1, 0
 2600, Vehicles, 300, -10{0}, 0{0}, 1, CN1, 1, 0
 
//
// Blocking the Right Lane
//
 
 2040, Barrel, 260, -0.5{0}, 5, 0
 2040, Barrel, 265, -1.25{0}, 5, 0
 2040, Barrel, 270, -2.5{0}, 5, 0
 2040, Barrel, 280, -3.75{0}, 5, 0
 2040, Barrel, 292, -5{0}, 5, 0
 2040, Barrel, 305, -6.5{0}, 5, 0
 2040, Barrel, 325, -6.5{0}, 5, 0
 2040, Barrel, 345, -6.5{0}, 5, 0
 2040, Barrel, 365, -6.5{0}, 5, 0
 2040, Barrel, 385, -6.5{0}, 5, 0
 2040, Barrel, 405, -6.5{0}, 5, 0
 2040, Barrel, 425, -5{0}, 5, 0
 2040, Barrel, 435, -3.75{0}, 5, 0
 2040, Barrel, 440, -0.5{0}, 5, 0
 2040, Vehicles, 320, -3.5{0}, 0{0}, 1, CN4, 1, 0
 2040, Vehicles, 350, -3{0}, 0{0}, 1, CN1, 1, 0
 2040, Vehicles, 300, -3{0}, 0{0}, 1, CN1, 1, 0
 
 
//
// Addiiton of Parked vehicles
//

20, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
100, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock4%
600, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock5%
700, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock4%
850, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
900, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
1100, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock4%
1300, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock5%
1200, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
1400, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock4%
2500, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock4%
2500, Static Object, 150, -0{2}, 0, 0, 0, 0, %CarBlock5%
2450, Static Object, 150, 0{1}, 0, 180, 0, 0, %CarBlock3%
2550, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock5%
3000, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock4%
3200, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock5%
3200, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock5%
4200, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
3700, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock4%
3900, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock5%
3950, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock5%
3900, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
4000, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock4%
4200, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock5%
4350, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock5%
4450, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
4600, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock4%
4800, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock5%
4930, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
5100, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock4%
5500, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock4%
5700, Static Object, 150, -0{2}, 0, 0, 0, 0, %CarBlock5%
6050, Static Object, 150, 0{1}, 0, 180, 0, 0, %CarBlock3%
6350, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock5%
6900, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock5%
7200, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock5%
7300, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
7200, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock4%
7800, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock5%
7850, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock5%
7950, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
7975, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock4%
8250, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock5%
8300, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock5%
8450, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%

//
// Adding Lamp posts set every 80m at spaces to avoid overlapping intersections
// Left side lamp posts
//
 0, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 560, 80 
 720, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 1100, 80 
 1810, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 0, 0
 1920, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 1110, 80 
 3010, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 0, 0 
 3120, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 640, 80
 3610, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 0, 0
 3720, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 700, 80
 4460, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 1100, 80
 5920, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 1030, 80 
 6910, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 0, 0 
 7040, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 1190, 80
 8210, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 0, 0 
 8320, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 1200, 80 
 
//Right side lamp post
 40, Static Object, 100, 1.5{1}, 0, 180, 0, 0, %Streetlight%, 1040, 80 
 1240, Static Object, 100, 1.5{1}, 0, 180, 0, 0, %Streetlight%, 1110, 80
 2440, Static Object, 100, 1.5{1}, 0, 180, 0, 0, %Streetlight%, 1920, 80
 4410, Static Object, 100, 1.5{1}, 0, 180, 0, 0, %Streetlight%, 0, 0
 4520, Static Object, 100, 1.5{1}, 0, 180, 0, 0, %Streetlight%, 3030, 80
 7510, Static Object, 100, 1.5{1}, 0, 180, 0, 0, %Streetlight%, 0, 0
 7640, Static Object, 100, 1.5{1}, 0, 180, 0, 0, %Streetlight%, 1120, 80
 8800, Static Object, 100, 1.5{1}, 0, 180, 0, 0, %Streetlight%, 0, 0
 8920, Static Object, 100, 1.5{1}, 0, 180, 0, 0, %Streetlight%, 800, 80
 

//
// Adding in Buildings
//
// Left hand side - 1 0 to 480

0,	     Building	,	250	,	 -8{2}	,	H	19	,	0
35,	     Building	,	250	,	 -8{2}	,	H	15	,	0
75,	     Building	,	250	,	 -8{2}	,	H	9	,	0
105,	 Building	,	250	,	 -8{2}	,	H	18	,	0
145,	 Building	,	250	,	 -8{2}	,	H	5	,	0
190,	 Building	,	250	,	 -8{2}	,	H	15	,	0
225,	 Building	,	250	,	 -8{2}	,	H	4	,	0
270,	 Building	,	250	,	 -8{2}	,	H	1	,	0
315,	 Building	,	250	,	 -8{2}	,	H	16	,	0
370,	 Building	,	250	,	 -8{2}	,	H	18	,	0
405,	 Building	,	250	,	 -8{2}	,	H	9	,	0
465,	 Building	,	250	,	 -8{2}	,	B	5	,	0
480,	 Building	,	250	,	 -60{2}	,	B	12	,	270
480,	 Building	,	250	,	 -95{2}	,	H	12	,	270
480,	 Building	,	250	,	 -115{2},	H	22	,	270
480,	 Building	,	250	,	 -140{2},	H	19	,	270
480,	 Building	,	250	,	 -195{2},	H   3	,	270
480,	 Building	,	250	,	 -225{2},	H	23	,	270

/// 2 suburbs 540 to 960///

520,	 Building	,	250	,	 -65{2}	,	H	1	,	90
520,	 Building	,	250	,	 -100{2},	H	18	,	90
520,	 Building	,	250	,	 -135{2},	H	17	,	90
520,	 Building	,	250	,	 -180{2},	H	6	,	90
520,	 Building	,	250	,	 -210{2},	H	11	,	90
520,	 Building	,	250	,	 -235{2},	H	22	,	90
534,	 Building	,	250	,	 -8{2}	,	B	10	,	0
575,	 Building	,	250	,	 -8{2}	,	H	12	,	0
610,	 Building	,	250	,	 -8{2}	,	B	9	,	0
650,	 Building	,	250	,	 -8{2}	,	H	2	,	0
695,	 Building	,	250	,	 -8{2}	,	H	16	,	0
720,	 Building	,	250	,	 -8{2}	,	H	11	,	0
765,	 Building	,	250	,	 -8{2}	,	H	7	,	0
800,	 Building	,	250	,	 -8{2}	,	H	20	,	0
830,	 Building	,	250	,	 -8{2}	,	H	17	,	0
875,	 Building	,	250	,	 -8{2}	,	H	23	,	0
900,	 Building	,	250	,	 -8{2}	,	H	14	,	0
930,	 Building	,	250	,	 -8{2}	,	H	8	,	0
980,	 Building	,	250	,	 -15{2}	,	H	11	,	270
980,	 Building	,	250	,	 -45{2}	,	H	16	,	270
980,	 Building	,	250	,	 -95{2}	,	B	9	,	270
980,	 Building	,	250	,	 -130{2},	B	7	,   270
980,	 Building	,	250	,	 -180{2},	H	17	,	270
980,	 Building	,	250	,	 -215{2},	B	3	,	270

// 3 suburbs 1040 to 1660///

1054,	 Building	,	250	,	 -8{2}	,	B	16	,	0
1020,	 Building	,	250	,	 -75{2}	,	H	9	,	90
1020,	 Building	,	250	,	 -115{2},	H	6	,	90
1020,	 Building	,	250	,	 -145{2},	H	1	,	90
1020,	 Building	,	250	,	 -175{2},	H	13	,	90
1020,	 Building	,	250	,	 -210{2},	H	19	,	90
1090, 	 Building	,	250	,	 -8{2}	,	H	7	,	0
1125,	 Building	,	250	,	 -8{2}	,	H	19	,	0
1155,	 Building	,	250	,	 -8{2}	,	H	17	,	0
1190,	 Building	,	250	,	 -8{2}	,	H	16	,	0
1225,	 Building	,	250	,	 -8{2}	,	H	15	,	0
1255,	 Building	,	250	,	 -8{2}	,	H	10	,	0
1285,	 Building	,	250	,	 -8{2}	,	H	23	,	0
1320,	 Building	,	250	,	 -8{2}	,	H	7	,	0
1355,	 Building	,	250	,	 -8{2}	,	H	2	,	0
1390,	 Building	,	250	,	 -8{2}	,	H	11	,	0
1435,	 Building	,	250	,	 -8{2}	,	B	4	,	0
1465,	 Building	,	250	,	 -8{2}	,	B	8	,	0
1495,	 Building	,	250	,	 -8{2}	,	B	7	,	0
1525,	 Building	,	250	,	 -8{2}	,	H	2	,	0
1580,	 Building	,	250	,	 -8{2}	,	B	1	,	0
1680,	 Building	,	250	,	 -220{2},	B	12	,	270
1680,	 Building	,	250	,	 -185{2},	B	7	,	270
1680,	 Building	,	250	,	 -138{2},	B	8	,	270
1680,	 Building	,	250	,	 -100{2},	B	2	,	270
1680,	 Building	,	250	,	 -75{2}	,	B	3	,	270
1680,	 Building	,	250	,	 -40{2}	,	B	11	,	270
1652,	 Building	,	250	,	 -8{2}	,	B	10	,	0

/// 4 suburbs 1740 to 2160///

1730,	 Building	,	250	,	 -8{2}	,	H	6	,	0
1720,	 Building	,	250	,	 -45{2}	,	H	11	,	90
1720,	 Building	,	250	,	 -80{2}	,	H	9	,	90
1720,	 Building	,	250	,	 -115{2},	H	16	,	90
1720,	 Building	,	250	,	 -150{2},	H	12	,	90
1720,	 Building	,	250	,	 -200{2},	H	2	,	90
1825,	 Building	,	250	,	 -8{2}	,	H	18	,	0
1885,	 Building	,	250	,	 -8{2}	, 	H	7	,	0
1930, Static Object ,   250 ,    -8{2}  ,   0       ,   90   ,  0  ,  0  , %Housegroup%  , 0
1990,	 Building	,	250	,	 -8{2}	,	H	16	,	0
2025,	 Building	,	250	,	 -8{2}	,	H	13	,	0
2070,	 Building	,	250	,	 -8{2}	,	H	22	,	0
2115,	 Building	,	250	,	 -8{2}	,	H	3	,	0
2170,	 Building	,	250	,	 -8{2}	,	B	8	,	0
2180,	 Building	,	250	,	 -35{2}	,	B	6	,	270
2180,	 Building	,	250	,	 -85{2}	,	H	8	,	270
2180,	 Building	,	250	,	 -115{2},	H	16	,	270
2180,	 Building	,	250	,	 -150{2},	H	20	,	270
2180,	 Building	,	250	,	 -200{2},	H	23	,	270

/// 5 Suburbs 2240 to 2860///

2235,	 Building	,	250	,	 -8{2}	,	H	22	,	0
2220,	 Building	,	250	,	 -40{2}	,	H	7	,	90
2220,	 Building	,	250	,	 -75{2}	,	H	2	,	90
2220,	 Building	,	250	,	 -105{2},	H	13	,	90
2220,	 Building	,	250	,	 -145{2},	H	21	,	90
2220,	 Building	,	250	,	 -190{2},	H	6	,	90
2220,	 Building	,	250	,	 -215{2},	H	9	,	90
2295,	 Building	,	250	,	 -8{2}	,	H	17	,	0	
2325,	 Building	,	250	,	 -8{2}	,	H	6	,	0	
2350,	 Building	,	250	,	 -8{2}	,	H	13	,	0
2395,	 Building	,	250	,	 -8{2}	,	H	14	,	0	
2432,	 Building	,	250	,	 -8{2}	,	H	4	,	0
2470,	 Building	,	250	,	 -8{2}	,	H	6	,	0		
2580, Static Object ,   250 ,    1.5{2} ,   0       ,   90   ,  0  ,  0  , %Housegroup%  , 0
2695,	 Building	,	250	,	 -8{2}	,   B	12	,	0
2750,	 Building	,	250	,	 -8{2}	,	B	26	,	0
2850,	 Building	,	250	,	 -8{2}	,	B	23	,	0
2880,	 Building	,	250	,	 -55{2}	,	B	17	,	270
2880,	 Building	,	250	,	 -115{2},	B	21	,	270
2880,	 Building	,	250	,	 -190{2},	B	16	,	270

/// 6 Warehouse or industrial area 2940 to 3560 ///

2920,	 Building	,	250	,	 -160{2},	B	6	,	90
2920,	 Building	,	250	,	 -100{2},	B	20	,	90
2920,	 Building	,	250	,	 -40{2}	,	B	16	,	90
3035,	 Building	,	250	,	 -8{2}	,	B	22	,	0
3120,	 Building	,	250	,	 -8{2}	,	B	18	,	0
3230,	 Building	,	250	,	 -8{2}	,	B	17	,	0
3305,	 Building	,	250	,	 -8{2}	,	B	21	,	0
3380,	 Building	,	250	,	 -8{2}	,	B	9	,	0
3420,	 Building	,	250	,	 -8{2}	,	B	18	,	0
3475,	 Building	,	250	,	 -8{2}	,	B	10	,	0
3574,	 Building	,	250	,	 -8{2}	,	B	13	,	0
3580,	 Building	,	250	,	 -60{2}	,	B	19	,	270
3580,	 Building	,	250	,	 -120{2},	G	13	,	270
3580,	 Building	,	250	,	 -205{2},	G	60	,	270

/// 7 Industrial or apartments 3640 to 4260///	
										
3640,	 Building	,	250	,	 -8{2}	,	B	7	,	0
3620,	 Building	,	250	,	 -70{2}	,	G	51	,	90
3620,	 Building	,	250	,	 -150{2},	G	53	,	90
3620,	 Building	,	250	,	 -210{2},	B	20	,	90
3710,	 Building	,	250	,	 -8{2}	,	G	40	,	0
3799,	 Building	,	250	,	 -8{2}	,	G	43	,	0
3870,	 Building	,	250	,	 -8{2}	,	G	52	,	0
3940,	 Building	,	250	,	 -8{2}	,	G	35	,	0
3995,	 Building	,	250	,	 -8{2}	,	B	9	,	0
4035,	 Building	,	250	,	 -8{2}	,	G	21	,	0
4080,	 Building	,	250	,	 -8{2}	,	B	4	,	0
4145,	 Building	,	250	,	 -8{2}	,	G	45	,	0
4205,	 Building	,	250	,	 -8{2}	,	B	5	,	0
4260,	 Building	,	250	,	 -8{2}	,	B	12	,	0
4280,	 Building	,	250	,	 -45{2}	,	G   56	,	270
4280,	 Building	,	250	,	 -98{2}	,	G	64	,	270
4280,	 Building	,	250	,	 -155{2},	G	48	,	270
4280,	 Building	,	250	,	 -210{2},	G	43	,	270

/// 8 Apartments and offices 4340 to 5660///				
							
4350,	 Building	,	250	,	 -8{2}	,	G	49	,	0
4320,	 Building	,	250	,	 -50{2}	,	G	68	,	90
4320,	 Building	,	250	,	 -95{2}	,	G	1	,	90
4320,	 Building	,	250	,	 -155{2},	G	10	,	90
4320,	 Building	,	250	,	 -205{2},	G	44	,	90
4420,	 Building	,	250	,	 -8{2}	,	G	13	,	0
4455,	 Building	,	250	,	 -8{2}	,	S	9	,	0
4490,	 Building	,	250	,	 -8{2}	,	S	7	,	0
4535,	 Building	,	250	,	 -8{2}	,	S	6	,	0
4600,	 Building	,	250	,	 -8{2}	,	G	47	,	0
4655,	 Building	,	250	,	 -8{2}	,	S	6	,	0
4690,	 Building	,	250	,	 -8{2}	,	G	69	,	0
4755,	 Building	,	250	,	 -8{2}	,	B	4	,	0
4785,	 Building	,	250	,	 -8{2}	,	G	26	,	0
4835,	 Building	,	250	,	 -8{2}	,	B	12	,	0
4880,	 Building	,	250	,	 -8{2}	,	G	3	,	0
4935,	 Building	,	250	,	 -8{2}	,	B	5	,	0
4975,	 Building	,	250	,	 -8{2}	,	S	3	,	0
5050,	 Building	,	250	,	 -8{2}	,	S	15	,	0
5145,	 Building	,	250	,	 -8{2}	,	G	49	,	0
5200,	 Building	,	250	,	 -8{2}	,	S	3	,	0
5250,	 Building	,	250	,	 -8{2}	,	B	1	,	0
5295,	 Building	,	250	,	 -8{2}	,	G	7	,	0
5345,	 Building	,	250	,	 -8{2}	,	B	8	,	0
5395,	 Building	,	250	,	 -8{2}	,	G	31	,	0
5510,	 Building	,	250	,	 -8{2}	,	G	20	,	0
5585,	 Building	,	250	,	 -8{2}	,	B	9	,	0
5610,	 Building	,	250	,	 -8{2}	,	B	15	,	0
5680,	 Building	,	250	,	 -40{2}	,	G	63	,	270
5680,	 Building	,	250	,	 -125{2},	G	45	,	270
5680,	 Building	,	250	,	 -220{2},	G	60	,	270

///9 Offices 5740 to 6760 ///	
										
5720,	 Building	,	250	,	 -40{2}	,	G	37	,	90
5720,	 Building	,	250	,	 -180{2},	G   9	,	90
5855,	 Building	,	250	,	 -8{2}	,	G	11	,	0
5890,	 Building	,	250	,	 -8{2}	,	B	4	,	0
5915,	 Building	,	250	,	 -8{2}	,	G	25	,	0
5950,	 Building	,	250	,	 -8{2}	,	G	12	,	0
6055,	 Building	,	250	,	 -8{2}	,	G	42	,	0
6175,	 Building	,	250	,	 -8{2}	,	G	51	,	0
6245,	 Building	,	250	,	 -8{2}	,	G	26	,	0
6295,	 Building	,	250	,	 -8{2}	,	S	12	,	0
6360,	 Building	,	250	,	 -8{2}	,	B	5	,	0
6440,	 Building	,	250	,	 -8{2}	,	G	23	,	0
6500,	 Building	,	250	,	 -8{2}	,	G	1	,	0
6545,	 Building	,	250	,	 -8{2}	,	B	8	,	0
6615,	 Building	,	250	,	 -8{2}	,	G	34	,	0
6695,	 Building	,	250	,	 -8{2}	,	G	19	,	0
6780,	 Building	,	250	,	 -32{2}	,	G	6	,	270
6780,	 Building	,	250	,	 -100{2},	G	11	,	270
6780,	 Building	,	250	,	 -155{2},	G	22  ,	270
6780,	 Building	,	250	,	 -205{2},	G	4	,	270

/// 10 Offices to warehouses 6840 to 7360 ///											

6848,	 Building	,	250	,	 -8{2}	,	S	12	,	0
6820,	 Building	,	250	,	 -55{2}	,	S	17	,	90
6820,	 Building	,	250	,	 -95{2}	,	S	8	,	90
6820,	 Building	,	250	,	 -140{2},	S	2	,	90
6820,	 Building	,	250	,	 -195{2},	S	9	,	90
6820,	 Building	,	250	,	 -230{2},	S	4	,	90
6920,	 Building	,	250	,	 -8{2}	,	S	11	,	0
6990,	 Building	,	250	,	 -8{2}	,	G	37	,	0
7040,	 Building	,	250	,	 -8{2}	,	S	9	,	0
7100,	 Building	,	250	,	 -8{2}	,	G	2	,	0
7190,	 Building	,	250	,	 -8{2}	,	G	65	,	0
7250,	 Building	,	250	,	 -8{2}	,	S	3	,	0
7300,	 Building	,	250	,	 -8{2}	,	G	31	,	0
7355,	 Building	,	250	,	 -8{2}	,	G	28	,	0
7380,	 Building	,	250	,	 -70{2}	,	G	8	,	270
7380,	 Building	,	250	,	 -170{2},	G	2	,	270

/// 11 Apartments 7440 to 8060 ///											

7443,	 Building	,	250	,	 -8{2}	,	G	65	,	0
7420,	 Building	,	250	,	 -80{2}	,	G	57	,	90
7420,	 Building	,	250	,	 -147{2},	G	61	,	90
7420,	 Building	,	250	,	 -190{2},	G	41	,	90
7490,	 Building	,	250	,	 -8{2}	,	G	44	,	0
7545,	 Building	,	250	,	 -8{2}	,	G	60	,	0
7580,	 Building	,	250	,	 -8{2}	,	G	67	,	0
7640,	 Building	,	250	,	 -8{2}	,	G	52	,	0
7705,	 Building	,	250	,	 -8{2}	,	G	58	,	0
7785,	 Building	,	250	,	 -8{2}	,	B	13	,	0
7845,	 Building	,	250	,	 -8{2}	,	G	57	,	0
7925,	 Building	,	250	,	 -8{2}	,	G	53	,	0
8005,	 Building	,	250	,	 -8{2}	,	G	56	,	0
8065,	 Building	,	250	,	 -8{2}	,	B	5	,	0
8080,	 Building	,	250	,	 -45{2}	,	G	5	,	270
8080,	 Building	,	250	,	 -85{2}	,	B	2	,	270
8080,	 Building	,	250	,	 -125{2},	B	7	,	270
8080,	 Building	,	250	,	 -175{2},	H	2	,	270
8080,	 Building	,	250	,	 -220{2},	H	9	,	270

/// 12 Shops and suburbs 8140 to 8660 ///											

8145,	 Building	,	250	,	 -8{2}	,	B	6	,	0
8120,	 Building	,	250	,	 -70{2}	,	B	11	,	90
8120,	 Building	,	250	,	 -130{2},	H	7	,	90
8120,	 Building	,	250	,	 -175{2},	H	13	,	90
8120,	 Building	,	250	,	 -220{2},	H	5	,	90
8190,	 Building	,	250	,	 -8{2}	,	B	3	,	0
8220,	 Building	,	250	,	 -8{2}	,	H	14	,	0
8255,	 Building	,	250	,	 -8{2}	,	H	8	,	0
8290,	 Building	,	250	,	 -8{2}	,	B	6	,	0
8325,	 Building	,	250	,	 -8{2}	,	B	9	,	0
8370,	 Building	,	250	,	 -8{2}	,	H	17	,	0
8405,	 Building	,	250	,	 -8{2}	,	H	2	,	0
8470,	 Building	,	250	,	 -8{2}	,	H	9	,	0
8535,	 Building	,	250	,	 -8{2}	,	B	11	,	0
8580,	 Building	,	250	,	 -8{2}	,	B	12	,	0
8625,	 Building	,	250	,	 -8{2}	,	B	3	,	0
8665,	 Building	,	250	,	 -8{2}	,	B	9	,	0
8680,	 Building	,	250	,	 -50{2}	,	B	13	,	270
8680,	 Building	,	250	,	 -95{2}	,	H	13	,	270
8680,	 Building	,	250	,	 -145{2},	H	10	,	270
8680,	 Building	,	250	,	 -205{2},	H	3	,	270

/// 13 suburbs 8740 to 9200 ///				
							
8740,	 Building	,	250	,	 -8{2}	,	H	21	,	0
8720,	 Building	,	250	,	 -45{2}	,	H	16	,	90
8720,	 Building	,	250	,	 -95{2}	,	H	12	,	90
8720,	 Building	,	250	,	 -120{2},	H	7	,	90
8720,	 Building	,	250	,	 -190{2},	H	2	,	90
8790,	 Building	,	250	,	 -8{2}	,	B	15	,	0
8820,	 Building	,	250	,	 -8{2}	,	H	11	,	0
8855,	 Building	,	250	,	 -8{2}	,	H	20	,	0
8890,	 Building	,	250	,	 -8{2}	,	B	13	,	0
8915,	 Building	,	250	,	 -8{2}	,	H	4	,	0
8950,	 Building	,	250	,	 -8{2}	,	H	7	,	0
8990,	 Building	,	250	,	 -8{2}	,	H	19	,	0
9020,	 Building	,	250	,	 -8{2}	,	H	8	,	0
9055,	 Building	,	250	,	 -8{2}	,	H	19	,	0
9090,	 Building	,	250	,	 -8{2}	,	H 	15	,	0
9115,	 Building	,	250	,	 -8{2}	,	H	8	,	0



// Right hand side - 1 0 to 480																					
//																					
																					
00	,	 Building	,	250	,	 8{1}	,	H	12	,	0										
70	,	 Building	,	250	,	 8{1}	,	H	19	,	0										
115	,	 Building	,	250	,	 8{1}	,	H	3	,	0										
155	,	 Building	,	250	,	 8{1}	,	H	11	,	0																				
270	,	 Static Object,	250	,	 -1.2{1},	0	,	270,	0,	0,	%Housegroup%,	0				
350	,	 Building	,	250	,	 8{1}	,	H	9	,	0										
390	,	 Building	,	250	,	 8{1}	,	H	1	,	0										
435	,	 Building	,	250	,	 8{1}	,	H	5	,	0										
470	,	 Building	,	250	,	 8{1}	,	B	4	,	0																			
480	,	 Building	,	250	,	 32{1}	,	H	4	,	90										
480	,	 Building	,	250	,	 70{1}	,	H	14	,	90										
480	,	 Building	,	250	,	 100{1}	,	H	4	,	90										
480	,	 Building	,	250	,	 135{1}	,	H	11	,	90										
480	,	 Building	,	250	,	 160{1}	,	H	4	,	90										
480	,	 Building	,	250	,	 195{1}	,	H	13	,	90										
480	,	 Building	,	250	,	 230{1}	,	H	23	,	90	
									
 /// 2 suburbs 540 to 960///						
 
530	,	 Building	,	250	,	 8{1}	,	H	14	,	0										
520	,	 Building	,	250	,	 40{1}	,	H	21	,	270										
520	,	 Building	,	250	,	 75{1}	,	H	4	,	270										
520	,	 Building	,	250	,	 105{1}	,	H	8	,	270										
520	,	 Building	,	250	,	 130{1}	,	H	15	,	270		
520	,	 Building	,	250	,	 170{1}	,	H	13	,	270		
520	,	 Building	,	250	,	 210{1}	,	H	9	,	270										
630	,	 Static Object,	250	,   -1.5{1}	,	0		,	270	,	0	,	0	,	 %Housegroup%	,	0											
755	,	 Building	,	250	,	 8{1}	,	H	16	,	0										
795	,	 Building	,	250	,	 8{1}	,	H	22	,	0										
855	,	 Building	,	250	,	 8{1}	,	H	3	,	0										
890	,	 Building	,	250	,	 8{1}	,	H	6	,	0										
925	,	 Building	,	250	,	 8{1}	,	H	18	,	0										
975	,	 Building	,	250	,	 8{1}	,	H	19	,	0
980	,	 Building	,	250	,	 45{1}	,	H	12	,	90
980	,	 Building	,	250	,	 80{1}	,	H	9	,	90
980	,	 Building	,	250	,	 125{1}	,	H	5	,	90
980	,	 Building	,	250	,	 175{1}	,	H	2	,	90
980	,	 Building	,	250	,	 215{1}	,	H	15	,	90

/// 3 suburbs 1040 to 1660 ///	
																				
1033,	 Building	,	250	,	 8{1}	,	B	10	,	0
1020,	 Building	,	250	,	 45{1}	,	B	7	,	270	
1020,	 Building	,	250	,	 90{1}	,	B	6	,	270	
1020,	 Building	,	250	,	 135{1}	,	H	14	,	270	
1020,	 Building	,	250	,	 170{1}	,	H	12	,	270	
1020,	 Building	,	250	,	 210{1}	,	H	20	,	270											
1080,	 Building	,	250	,	 8{1}	,	H	19	,	0										
1130,	 Building	,	250	,	 8{1}	,	H	18	,	0										
1205,	 Building	,	250	,	 8{1}	,	H	8	,	0										
1250,	 Building	,	250	,	 8{1}	,	H	20	,	0										
1295,	 Building	,	250	,	 8{1}	,	B	10	,	0										
1325,	 Building	,	250	,	 8{1}	,	B	3	,	0										
1355,	 Building	,	250	,	 8{1}	,	H	22	,	0										
1390,	 Building	,	250	,	 8{1}	,	H	1	,	0										
1435,	 Building	,	250	,	 8{1}	,	H	3	,	0										
1470,	 Building	,	250	,	 8{1}	,	B	7	,	0										
1530,	 Building	,	250	,	 8{1}	,	B	1	,	0										
1580,	 Building	,	250	,	 8{1}	,	H	13	,	0										
1615,	 Building	,	250	,	 8{1}	,	H	12	,	0										
1665,	 Building	,	250	,	 8{1}	,	B	11	,	0	
1680,	 Building	,	250	,	 45{1}	,	B	5	,	90
1680,	 Building	,	250	,	 85{1}	,	B	2	,	90
1680,	 Building	,	250	,	 120{1}	,	H	9	,	90
1680,	 Building	,	250	,	 175{1}	,	H	11	,	90
1680,	 Building	,	250	,	 215{1}	,	H	2	,	90
									
/// 4 suburbs 1740 to 2160 ///			
																		
1738,	 Building	,	250	,	 8{1}	,	B	4	,	0	
1720,	 Building	,	250	,	 90{1}	,	B   8   ,	270
1720,	 Building	,	250	,	 115{1}	,	B	7	,	270									
1720,	 Building	,	250	,	 150{1}	,	B	9	,	270
1720,	 Building	,	250	,	 190{1}	,	H	2	,	270
1720,	 Building	,	250	,	 215{1}	,	H	1	,	270			
1780,	 Building	,	250	,	 8{1}	,	B	14	,	0	
1840,	 Building	,	250	,	 8{1}	,	B	16	,	0										
1910,	 Building	,	250	,	 8{1}	,	H	6	,	0										
1940,	 Building	,	250	,	 8{1}	,	H	16	,	0										
1975,	 Building	,	250	,	 8{1}	,	H	14	,	0										
2000,	 Building	,	250	,	 8{1}	,	H	22	,	0										
2025,	 Building	,	250	,	 8{1}	,	H	23	,	0										
2055,	 Building	,	250	,	 8{1}	,	B	8	,	0										
2090,	 Building	,	250	,	 8{1}	,	B	11	,	0										
2130,	 Building	,	250	,	 8{1}	,	H	8	,	0										
2170,	 Building	,	250	,	 8{1}	,	B	6	,	0	
2180,	 Building	,	250	,	 45{1}	,	B	10	,	90	
2180,	 Building	,	250	,	 100{1}	,	B	2	,	90	
2180,	 Building	,	250	,	 135{1}	,	H	15	,	90	
2180,	 Building	,	250	,	 175{1}	,	H	19	,	90	
2180,	 Building	,	250	,	 210{1}	,	H	5	,	90	
									
///  5 Suburbs 2240 to 2860///				
																	
2235,	 Building	,	250	,	 8{1}	,	B	1	,	0
2220,	 Building	,	250	,	 60{1}	,	B	4	,	270
2220,	 Building	,	250	,	 95{1}	,	B	9	,	270
2220,	 Building	,	250	,	 130{1}	,	H	11	,	270	
2220,	 Building	,	250	,	 175{1}	,	H	18	,	270	
2220,	 Building	,	250	,	 220{1}	,	B	5	,	270								
2280,	 Building	,	250	,	 8{1}	,	B	7	,	0										
2310,	 Building	,	250	,	 8{1}	,	H	11	,	0										
2340,	 Building	,	250	,	 8{1}	,	H	15	,	0										
2375,	 Building	,	250	,	 8{1}	,	H	13	,	0										
2400,	 Building	,	250	,	 8{1}	,	H	7	,	0										
2425,	 Building	,	250	,	 8{1}	,	H	12	,	0										
2455,	 Building	,	250	,	 8{1}	,	H	9	,	0										
2490,	 Building	,	250	,	 8{1}	,	H	7	,	0										
2530,	 Building	,	250	,	 8{1}	,	H	16	,	0										
2565,	 Building	,	250	,	 8{1}	,	H	21	,	0																				
2625,	 Building	,	250	,	 8{1}	,	H	23	,	0										
2655,	 Building	,	250	,	 8{1}	,	H	12	,	0										
2690,	 Building	,	250	,	 8{1}	,	H	6	,	0										
2730,	 Building	,	250	,	 8{1}	,	H	5	,	0										
2755,	 Building	,	250	,	 8{1}	,	H	9	,	0										
2795,	 Building	,	250	,	 8{1}	,	B	10	,	0										
2820,	 Building	,	250	,	 8{1}	,	B	1	,	0
2870,	 Building	,	250	,	 8{1}	,	B	9	,	0										
2880,	 Building	,	250	,	 45{1}	,	B	11	,	90
2880,	 Building	,	250	,	 95{1}	,	B	2	,	90
2880,	 Building	,	250	,	 135{1}	,	B	6	,	90
2880,	 Building	,	250	,	 180{1}	,	B	8	,	90
2880,	 Building	,	250	,	 215{1}	,	B	7	,	90	
									
/// 6 Warehouse or industrial area 2940 to 3560 ///		
																			
2930,	 Building	,	250	,	 8{1}	,	B	1	,	0
2920,	 Building	,	250	,	 40{1}	,	H	12	,	270												
2920,	 Building	,	250	,	 75{1}	,	B	5	,	270													
2920,	 Building	,	250	,	 125{1}	,	B	3	,	270										
2920,	 Building	,	250	,	 170{1}	,	B	4	,	270										
2920,	 Building	,	250	,	 210{1}	,	B	12	,	270																		
2980,	 Building	,	250	,	 8{1}	,	H	3	,	0										
3005,	 Building	,	250	,	 8{1}	,	H	2	,	0										
3035,	 Building	,	250	,	 8{1}	,	H	16	,	0										
3050,	 Building	,	250	,	 8{1}	,	B	23	,	0										
3095,	 Building	,	250	,	 8{1}	,	B	26	,	0																				
3165,	 Building	,	250	,	 8{1}	,	B	21	,	0																														
3230,	 Building	,	250	,	 8{1}	,	G	66	,	0																																						
3290,	 Building	,	250	,	 8{1}	,	B	12	,	0																																												
3370,	 Building	,	250	,	 8{1}	,	G	50	,	0										
3450,	 Building	,	250	,	 8{1}	,	G	47	,	0													
3552,	 Building	,	250	,	 8{1}	,	G	48	,	0	
3580,	 Building	,	250	,	 100{1}	,	G	39	,	90	
3580,	 Building	,	250	,	 200{1}	,	G	56	,	90
									
/// 7 Industrial or apartments 3640 to 4260 ///			
																		
3630,	 Building	,	250	,	 55{1}	,	G	48	,	270		
3620,	 Building	,	250	,	 115{1}	,	G	25	,	270	
3620,	 Building	,	250	,	 170{1}	,	G	36	,	270	
3620,	 Building	,	250	,	 215{1}	,	G	61	,	270																																						
3740,	 Building	,	250	,	 8{1}	,	G	70	,	0										
3830,	 Building	,	250	,	 8{1}	,	G	50	,	0																													
3900,	 Building	,	250	,	 8{1}	,	B	10	,	0										
3960,	 Building	,	250	,	 8{1}	,	G	56	,	0										
4000,	 Building	,	250	,	 8{1}	,	G	62	,	0																				
4080,	 Building	,	250	,	 8{1}	,	G	67	,	0																				
4155,	 Building	,	250	,	 8{1}	,	G	46	,	0																			
4210,	 Building	,	250	,	 8{1}	,	S	3	,	0																			
4280,	 Building	,	250	,	 70{1}	,	G	44	,	90	
4280,	 Building	,	250	,	 150{1}	,	S	6	,	90
4280,	 Building	,	250	,	 210{1}	,	G	21	,	90
									
/// 8 Apartments and offices 4340 to 5660 ///			
																		
4345,	 Building	,	250	,	 8{1}	,	G	60	,	0
4320,	 Building	,	250	,	 65{1}	,	G	54	,	270		
4320,	 Building	,	250	,	 145{1}	,	G	63	,	270	
4320,	 Building	,	250	,	 205{1}	,	G	39	,	270										
4395,	 Building	,	250	,	 8{1}	,	B	10	,	0																			
4460,	 Building	,	250	,	 8{1}	,	S	14	,	0										
4505,	 Building	,	250	,	 8{1}	,	G	26	,	0																												
4575,	 Building	,	250	,	 8{1}	,	G	32	,	0																				
4655,	 Building	,	250	,	 8{1}	,	G	13	,	0										
4720,	 Building	,	250	,	 8{1}	,	S	14	,	0										
4785,	 Building	,	250	,	 8{1}	,	G	68	,	0										
4845,	 Building	,	250	,	 8{1}	,	G	48	,	0																													
4950,	 Building	,	250	,	 8{1}	,	S	15	,	0										
5035,	 Building	,	250	,	 8{1}	,	G	40	,	0																					
5125,	 Building	,	250	,	 8{1}	,	G	33	,	0
5195,	 Building	,	250	,	 8{1}	,	S	29	,	0																																							
5275,	 Building	,	250	,	 8{1}	,	S	24	,	0																				
5345,	 Building	,	250	,	 8{1}	,	S	8	,	0																													
5460,	 Building	,	250	,	 8{1}	,	B	4	,	0																				
5525,	 Building	,	250	,	 8{1}	,	S	21	,	0																													
5680,	 Building	,	250	,	 35{1}	,	G	52	,	90		
5680,	 Building	,	250	,	 95{1}	,	G	49	,	90
5680,	 Building	,	250	,	 150{1}	,	G	61	,	90
5680,	 Building	,	250	,	 205{1}	,	G	55	,	90
									
/// 9 Offices 5740 to 6760 ///							
														
5760,	 Building	,	250	,	 8{1}	,	G	55	,	0	
5720,	 Building	,	250	,	 60{1}	,	G	53	,	270	
5720,	 Building	,	250	,	 135{1}	,	G	62	,	270
5720,	 Building	,	250	,	 205{1}	,	G	51	,	270																													
5850,	 Building	,	250	,	 8{1}	,	G	15	,	0																																																										
6040,	 Building	,	250	,	 8{1}	,	B	2	,	0										
6090,	 Building	,	250	,	 8{1}	,	G	41	,	0																				
6150,	 Building	,	250	,	 8{1}	,	B	12	,	0																				
6215,	 Building	,	250	,	 8{1}	,	S	10	,	0																													
6290,	 Building	,	250	,	 8{1}	,	G	64	,	0																			
6360,	 Building	,	250	,	 8{1}	,	G	18	,	0																				
6410,	 Building	,	250	,	 8{1}	,	B	9	,	0										
6455,	 Building	,	250	,	 8{1}	,	G	61	,	0																													
6550,	 Building	,	250	,	 8{1}	,	S	7	,	0										
6600,	 Building	,	250	,	 8{1}	,	S	18	,	0										
6655,	 Building	,	250	,	 8{1}	,	G	13	,	0										
6705,	 Building	,	250	,	 8{1}	,	B	4	,	0										
6765,	 Building	,	250	,	 45{1}	,	B	6	,	90	
6780,	 Building	,	250	,	 90{1}	,	B	12	,	90	
6780,	 Building	,	250	,	 130{1}	,	G	12	,	90	
6780,	 Building	,	250	,	 185{1}	,	G	6	,	90		
6780,	 Building	,	250	,	 210{1}	,	G	1	,	90	
									
/// 10 Offices 6840 to 7360 ///								
													
6820,	 Building	,	250	,	 35{1}	,	G	32	,	270	
6820,	 Building	,	250	,	 95{1}	,	G	48	,	270	
6820,	 Building	,	250	,	 140{1}	,	G	21	,	270		
6820,	 Building	,	250	,	 200{1}	,	B	9   ,	270									
6890,	 Building	,	250	,	 8{1}	,	S	3	,	0										
6920,	 Building	,	250	,	 8{1}	,	B	1	,	0										
6955,	 Building	,	250	,	 8{1}	,	B	8	,	0										
6990,	 Building	,	250	,	 8{1}	,	G	12	,	0										
7015,	 Building	,	250	,	 8{1}	,	G	44	,	0										
7070,	 Building	,	250	,	 8{1}	,	G	38	,	0																				
7135,	 Building	,	250	,	 8{1}	,	B	10	,	0																				
7175,	 Building	,	250	,	 8{1}	,	G	7	,	0										
7235,	 Building	,	250	,	 8{1}	,	G	60	,	0																			
7295,	 Building	,	250	,	 8{1}	,	B	9	,	0										
7315,	 Building	,	250	,	 8{1}	,	B	1	,	0										
7360,	 Building	,	250	,	 8{1}	,	B	12	,	0
7380,	 Building	,	250	,	 50{1}	,	B	8	,	90
7380,	 Building	,	250	,	 105{1}	,	G	60	,	90	
7380,	 Building	,	250	,	 170{1}	,	G   48	,	90		
									
/// 11 Apartments 7440 to 8060 ///						
															
7443,	 Building	,	250	,	 8{1}	,	G	64	,	0	
7420,	 Building	,	250	,	 65{1}	,	G	49	,	270
7420,	 Building	,	250	,	 150{1}	,	G	55	,	270
7420,	 Building	,	250	,	 220{1}	,	G	61	,	270									
7575,	 Building	,	250	,	 8{1}	,	G	40	,	0												
7685,	 Building	,	250	,	 8{1}	,	G	39	,	0	
7740,	 Building	,	250	,	 8{1}	,	G	46	,	0	
7820,	 Building	,	250	,	 8{1}	,	G	49	,	0																				
7890,	 Building	,	250	,	 8{1}	,	B	10	,	0																				
7950,	 Building	,	250	,	 8{1}	,	H	3	,	0										
7990,	 Building	,	250	,	 8{1}	,	H	5	,	0										
8020,	 Building	,	250	,	 8{1}	,	H	22	,	0										
8060,	 Building	,	250	,	 8{1}	,	B	7	,	0	
8080,	 Building	,	250	,	 45{1}	,	B	1	,	90	
8080,	 Building	,	250	,	 95{1}	,	B	11	,	90
8080,	 Building	,	250	,	 130{1}	,	H	10	,	90
8080,	 Building	,	250	,	 180{1}	,	B	3	,	90
8080,	 Building	,	250	,	 215{1}	,	B	5	,	90
									
/// 12 Shops and suburbs 8140 to 8660 ///					
																
8140,	 Building	,	250	,	 8{1}	,	B	10	,	0		
8120,	 Building	,	250	,	 45{1}	,	B	4	,	270	
8120,	 Building	,	250	,	 90{1}	,	B	9	,	270	
8120,	 Building	,	250	,	 135{1}	,	H   16	,	270	
8120,	 Building	,	250	,	 195{1}	,	H	19	,	270	
8120,	 Building	,	250	,	 220{1}	,	B	1	,	270									
8190,	 Building	,	250	,	 8{1}	,	B	2	,	0										
8220,	 Building	,	250	,	 8{1}	,	B	4	,	0										
8255,	 Building	,	250	,	 8{1}	,	H	12	,	0																			
8305,	 Building	,	250	,	 8{1}	,	H	20	,	0										
8350,	 Building	,	250	,	 8{1}	,	H	16	,	0										
8390,	 Building	,	250	,	 8{1}	,	B	8	,	0										
8430,	 Building	,	250	,	 8{1}	,	B	4	,	0
8470,	 Building	,	250	,	 8{1}	,	H	14	,	0
8505,	 Building	,	250	,	 8{1}	,	H	4	,	0										
8550,	 Building	,	250	,	 8{1}	,	B	9	,	0										
8590,	 Building	,	250	,	 8{1}	,	H	5	,	0										
8615,	 Building	,	250	,	 8{1}	,	H	23	,	0										
8655,	 Building	,	250	,	 8{1}	,	B	7	,	0
8680,	 Building	,	250	,	 45{1}	,	B	2	,	90	
8680,	 Building	,	250	,	 95{1}	,	H	5	,	90	
8680,	 Building	,	250	,	 130{1}	,	H	13	,	90	
8680,	 Building	,	250	,	 190{1}	,	H	17	,	90
8680,	 Building	,	250	,	 225{1}	,	H	22	,	90			
									
/// 13 suburbs 8740 to 9200 ///							
														
8740,	 Building	,	250	,	 8{1}	,	H	7	,	0										
8720,	 Building	,	250	,	 55{1}	,	H	15	,	270
8720,	 Building	,	250	,	 90{1}	,	H	12	,	270
8720,	 Building	,	250	,	 125{1}	,	B	4  ,	270
8720,	 Building	,	250	,	 175{1}	,	H	8	,	270
8720,	 Building	,	250	,	 210{1}	,	H	19	,	270
8790,	 Building	,	250	,	 8{1}	,	H	11	,	0										
8820,	 Building	,	250	,	 8{1}	,	B	2	,	0										
8855,	 Building	,	250	,	 8{1}	,	B	10	,	0										
8890,	 Building	,	250	,	 8{1}	,	H	21	,	0										
8915,	 Building	,	250	,	 8{1}	,	H	9	,	0										
8950,	 Building	,	250	,	 8{1}	,	H	6	,	0										
8990,	 Building	,	250	,	 8{1}	,	H	8	,	0										
9020,	 Building	,	250	,	 8{1}	,	H	3	,	0										
9055,	 Building	,	250	,	 8{1}	,	H	23	,	0										
9090,	 Building	,	250	,	 8{1}	,	H	1	,	0										
9115,	 Building	,	250	,	 8{1}	,	B	11	,	0										
						
 
 //
 // Additional Objects e.g. trash bins, park benches
 //
 
 0, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 500, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 950, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 1220, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 1420, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 1740, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 1840, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 2220, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 2600, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 3100, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 3500, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 7120, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 7520, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 7700, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 7950, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 8240, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 8500, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 
 100, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 560, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 790, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 1120, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 1350, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 1900, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 2150, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 2550, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 2950, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 3445, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 4060, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 4150, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 4190, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 4450, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 7240, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 7430, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 7750, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 7940, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 8590, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%