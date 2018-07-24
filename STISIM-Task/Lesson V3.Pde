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
SpeedLimit = C:\STISIM3\Data\EuroSigns\Speed Limit\
Sound = C:\STISIM3\Sound
CarBlock3 = C:\STISIM3\Data\Vehicles\Car_Block_3.mka
CarBlock4 = C:\STISIM3\Data\Vehicles\Car_Block_4.mka
CarBlock5 = C:\STISIM3\Data\Vehicles\Car_Block_5.mka
Streetlight = C:\STISIM3\Data\Miscellaneous\StreetLight_1.Mka
Staticobj = C:\STISIM3\Data\Buildings
Housegroup = C:\STISIM3\Data\Buildings\House\House_Group.Mka
Bench = C:\STISIM3\Data\Miscellaneous\Bench.mesh
ForeSlope1 = C:\Stisim3\Data\Textures\Grass_With_Flowers.Jpg
Lanes1 = C:\STISIM3\Data\Textures\Road15.Jpg
Shoulder1 = C:\Stisim3\Data\Textures\Dirt06.Jpg
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
	
 1700, ROADWAY, 12, 2, 1, 1, 0.5, 8, 10, 0.4, 0.4, 100, &
      0,      %Lanes1%,   0,12,12, &
      0,      %Lanes1%,   0,12,12, &
      0,  12, %Shoulder1%,  0,24,24, &
      0,  12, %Shoulder1%,  0,24,24, &
      0, 200, %Foreslope1%,  0,24,24, &
      0, 200, %Foreslope1%,  0,24,24, &
              %Foreslope1%,  0,24,24, &
              %Foreslope1%,  0,24,24, , , , ,
	
	

// -----------------------------------------------  SPEED LIMITS

0, Speed Limit, 54
0, Sign, 100, 100, %SpeedLimit%Speed_Limit_50.Mka ,  0, 0, 0
350, Sign, 100, 100, %SpeedLimit%Speed_Limit_50.Mka ,  0, 0, 0
950, Sign, 100, 100, %SpeedLimit%Speed_Limit_50.Mka ,  0, 0, 0
1770, Sign, 100, 100, %SpeedLimit%Speed_Limit_100.Mka ,  0, 0, 0
2200, Sign, 100, 100, %SpeedLimit%Speed_Limit_100.Mka ,  0, 0, 0
1900, Speed Limit, 104

//
//
// Add Traffic lights to all the intersections
//

 400, Signal Light, -250, 0, 0, 18, 2, -1.5, 6, 1, 0, 1, 0, 2.5
 700, Signal Light, -250, 100{5}, 3, 12, 0, -1.5, 6, 1, 0, 1, 0, 2.5
 1000, Signal Light, -250, 0, 0, 15, 2, -1.5, 6, 1, 0, 1, 0, 2.5
 1300, Signal Light, -250, 0, 0, 20, 2, -1.5, 6, 1, 0, 1, 0, 2.5

 
//
//    Add in Intersections 
//  Distance, Intersection, type, appear distance = 0 , turn permissions = 1, crosswalk type, crosswalk type, turn compliance, auditory warninng filepath, 
// A total of 15 intersections with 5 of each desired turn
// Randomised order of turns : Right, Straight, Right, Left, Right, Straight, Left, Right, Left, Straight, Left, Straight, Right, Left, Straight
//

 400.0	,	Intersection, 0, 250, 1, 2, 2, 1
 700.0	,	Intersection, 0, 250, 1, 2, 2, 2
 1000.0	,	Intersection, 0, 250, 1, 2, 2, 1
 1300.0	,	Intersection, 0, 250, 1, 2, 2, 2


//
//  ADD Audio Queues to signal the correct way through the intersection
//
 
 450 , Play Recording, %Sound%\NextRight.Wav, 0, 90
 750, Play Recording, %Sound%\NextLeft.Wav, 0, 90
 1050, Play Recording, %Sound%\NextRight.Wav, 0, 90
 1350, Play Recording, %Sound%\NextLeft.Wav, 0, 90

  
//  
// ADD in two construction zones
//
// Blocking Left lane

 1000, Barrel, 150, -11.5{0}, 5, 0
 1000, Barrel, 155, -11{0}, 5, 0
 1000, Barrel, 160, -10{0}, 5, 0
 1000, Barrel, 170, -9{0}, 5, 0
 1000, Barrel, 182, -8{0}, 5, 0
 1000, Barrel, 195, -7{0}, 5, 0
 1000, Barrel, 215, -7{0}, 5, 0
 1000, Barrel, 235, -7{0}, 5, 0
 1000, Barrel, 255, -7{0}, 5, 0
 1000, Barrel, 275, -7{0}, 5, 0
 1000, Barrel, 295, -7{0}, 5, 0
 1000, Barrel, 315, -8{0}, 5, 0
 1000, Barrel, 325, -10{0}, 5, 0
 1000, Barrel, 330, -11{0}, 5, 0
 1000, Vehicles, 210, -9.5{0}, 0{0}, 1, CN4, 1, 0
 1000, Vehicles, 240, -9{0}, 0{0}, 1, CN1, 1, 0
 1000, Vehicles, 190, -10{0}, 0{0}, 1, CN1, 1, 0
 
//
// Blocking the Right Lane
//
 
670, Barrel, 130, -0.5{0}, 5, 0
670, Barrel, 135, -1.25{0}, 5, 0
670, Barrel, 140, -2.5{0}, 5, 0
670, Barrel, 150, -3.75{0}, 5, 0
670, Barrel, 162, -5{0}, 5, 0
670, Barrel, 175, -6.5{0}, 5, 0
670, Barrel, 195, -6.5{0}, 5, 0
670, Barrel, 215, -6.5{0}, 5, 0
670, Barrel, 235, -6.5{0}, 5, 0
670, Barrel, 255, -6.5{0}, 5, 0
670, Barrel, 275, -6.5{0}, 5, 0
670, Barrel, 295, -5{0}, 5, 0
670, Barrel, 305, -3.75{0}, 5, 0
670, Barrel, 310, -0.5{0}, 5, 0
670, Vehicles, 190, -3.5{0}, 0{0}, 1, CN4, 1, 0
670, Vehicles, 220, -3{0}, 0{0}, 1, CN1, 1, 0
670, Vehicles, 170, -3{0}, 0{0}, 1, CN1, 1, 0
 
1280, Barrel, 130, -0.5{0}, 5, 0
1280, Barrel, 135, -1.25{0}, 5, 0
1280, Barrel, 140, -2.5{0}, 5, 0
1280, Barrel, 150, -3.75{0}, 5, 0
1280, Barrel, 162, -5{0}, 5, 0
1280, Barrel, 175, -6.5{0}, 5, 0
1280, Barrel, 195, -6.5{0}, 5, 0
1280, Barrel, 215, -6.5{0}, 5, 0
1280, Barrel, 235, -6.5{0}, 5, 0
1280, Barrel, 255, -6.5{0}, 5, 0
1280, Barrel, 275, -6.5{0}, 5, 0
1280, Barrel, 295, -5{0}, 5, 0
1280, Barrel, 305, -3.75{0}, 5, 0
1280, Barrel, 310, -0.5{0}, 5, 0
1280, Vehicles, 190, -3.5{0}, 0{0}, 1, CN4, 1, 0
1280, Vehicles, 220, -3{0}, 0{0}, 1, CN1, 1, 0
1280, Vehicles, 170, -3{0}, 0{0}, 1, CN1, 1, 0
  
 
//
// Additton of Parked vehicles
//

20, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
100, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock4%
320, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock5%
750, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock4%
850, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
900, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
1100, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock4%
1230, Static Object, 250, 0{1}, 0, 180, 0, 0, %CarBlock5%
1200, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock3%
1400, Static Object, 250, -0{2}, 0, 0, 0, 0, %CarBlock4%


//
// Adding Lamp posts set every 80m at spaces to avoid overlapping intersections
// Left side lamp posts
//

 0, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 560, 80 
 720, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 1100, 80 
 1810, Static Object, 100, -1.5{2}, 0, 0, 0, 0, %Streetlight%, 1300, 80

 
//Right side lamp post

 40, Static Object, 100, 1.5{1}, 0, 180, 0, 0, %Streetlight%, 1040, 80 
 1240, Static Object, 100, 1.5{1}, 0, 180, 0, 0, %Streetlight%, 1110, 80
 2440, Static Object, 100, 1.5{1}, 0, 180, 0, 0, %Streetlight%, 1920, 80
 4410, Static Object, 100, 1.5{1}, 0, 180, 0, 0, %Streetlight%, 0, 0


//
// Adding in Buildings
//
// Left hand side - 1 0 to 480

0,	     Building	,	250	,	 -8{2}	,	H	19	,	0
35,	     Building	,	250	,	 -8{2}	,	H	15	,	0
75,	     Building	,	250	,	 -8{2}	,	H	9	,	0
105,	 Building	,	250	,	 -8{2}	,	H	18	,	0
145,	 Building	,	250	,	 -8{2}	,	H	5	,	0
180,	 Building	,	250	,	 -8{2}	,	H	1	,	0
215,	 Building	,	250	,	 -8{2}	,	H	16	,	0
265,	 Building	,	250	,	 -8{2}	,	H	18	,	0
300,	 Building	,	250	,	 -8{2}	,	H	9	,	0
345,	 Building	,	250	,	 -8{2}	,	B	5	,	0
380,	 Building	,	250	,	 -60{2}	,	B	12	,	270
380,	 Building	,	250	,	 -95{2}	,	H	12	,	270
380,	 Building	,	250	,	 -115{2},	H	22	,	270
380,	 Building	,	250	,	 -140{2},	H	19	,	270
380,	 Building	,	250	,	 -195{2},	H   3	,	270
380,	 Building	,	250	,	 -225{2},	H	23	,	270

/// 2 suburbs 420 to 580///

420,	 Building	,	250	,	 -25{2}	,	B	4	,	90
420,	 Building	,	250	,	 -65{2}	,	H	1	,	90
420,	 Building	,	250	,	 -100{2},	H	18	,	90
420,	 Building	,	250	,	 -135{2},	H	17	,	90
420,	 Building	,	250	,	 -180{2},	H	6	,	90
420,	 Building	,	250	,	 -210{2},	H	11	,	90
420,	 Building	,	250	,	 -235{2},	H	22	,	90
475,	 Building	,	250	,	 -8{2}	,	H	12	,	0
610,	 Building	,	250	,	 -8{2}	,	B	9	,	0
640,	 Building	,	250	,	 -8{2}	,	H	2	,	0
680,	 Building	,	250	,	 -15{2}	,	H	11	,	270
680,	 Building	,	250	,	 -45{2}	,	H	16	,	270
680,	 Building	,	250	,	 -95{2}	,	B	9	,	270
680,	 Building	,	250	,	 -130{2},	B	7	,   270
680,	 Building	,	250	,	 -180{2},	H	17	,	270
680,	 Building	,	250	,	 -215{2},	B	3	,	270

// 3 suburbs 620 to 980///

756,	 Building	,	250	,	 -8{2}	,	B	16	,	0
720,	 Building	,	250	,	 -75{2}	,	H	9	,	90
720,	 Building	,	250	,	 -115{2},	H	6	,	90
720,	 Building	,	250	,	 -145{2},	H	1	,	90
720,	 Building	,	250	,	 -175{2},	H	13	,	90
720,	 Building	,	250	,	 -210{2},	H	19	,	90
790, 	 Building	,	250	,	 -8{2}	,	H	7	,	0
825,	 Building	,	250	,	 -8{2}	,	H	15	,	0
855,	 Building	,	250	,	 -8{2}	,	H	10	,	0
895,	 Building	,	250	,	 -8{2}	,	H	23	,	0
980,	 Building	,	250	,	 -220{2},	B	12	,	270
980,	 Building	,	250	,	 -185{2},	B	7	,	270
980,	 Building	,	250	,	 -138{2},	B	8	,	270
980,	 Building	,	250	,	 -100{2},	B	2	,	270
980,	 Building	,	250	,	 -75{2}	,	B	3	,	270
980,	 Building	,	250	,	 -40{2}	,	B	11	,	270
960,	 Building	,	250	,	 -8{2}	,	B	10	,	0

/// 4 suburbs 1020 to 1280///

1030,	 Building	,	250	,	 -8{2}	,	H	6	,	0
1020,	 Building	,	250	,	 -45{2}	,	H	11	,	90
1020,	 Building	,	250	,	 -80{2}	,	H	9	,	90
1020,	 Building	,	250	,	 -115{2},	H	16	,	90
1020,	 Building	,	250	,	 -150{2},	H	12	,	90
1020,	 Building	,	250	,	 -200{2},	H	2	,	90
1090,	 Building	,	250	,	 -8{2}	,	H	16	,	0
1125,	 Building	,	250	,	 -8{2}	,	H	13	,	0
1170,	 Building	,	250	,	 -8{2}	,	H	22	,	0
1215,	 Building	,	250	,	 -8{2}	,	H	3	,	0
1280,	 Building	,	250	,	 -20{2}	,	B	8	,	270
1280,	 Building	,	250	,	 -55{2}	,	B	6	,	270
1280,	 Building	,	250	,	 -85{2}	,	H	8	,	270
1280,	 Building	,	250	,	 -115{2},	H	16	,	270
1280,	 Building	,	250	,	 -150{2},	H	20	,	270
1280,	 Building	,	250	,	 -200{2},	H	23	,	270

/// 5 Suburbs 1320 to 1430///

1335,	 Building	,	250	,	 -8{2}	,	H	22	,	0
1320,	 Building	,	250	,	 -40{2}	,	H	7	,	90
1320,	 Building	,	250	,	 -75{2}	,	H	2	,	90
1320,	 Building	,	250	,	 -105{2},	H	13	,	90
1320,	 Building	,	250	,	 -145{2},	H	21	,	90
1320,	 Building	,	250	,	 -190{2},	H	6	,	90
1320,	 Building	,	250	,	 -215{2},	H	9	,	90
1395,	 Building	,	250	,	 -8{2}	,	H	17	,	0	





// Right hand side - 1 0 to 380																					
//																					
																					
00	,	 Building	,	250	,	 8{1}	,	H	12	,	0										
70	,	 Building	,	250	,	 8{1}	,	H	19	,	0										
115	,	 Building	,	250	,	 8{1}	,	H	3	,	0										
155	,	 Building	,	250	,	 8{1}	,	H	11	,	0										
170	,	 Building	,	250	,	 8{1}	,	H	15	,	0	
235	,	 Building	,	250	,	 8{1}	,	H	5	,	0										
275	,	 Building	,	250	,	 8{1}	,	B	4	,	0													
320	,	 Building	,	250	,	 8{1}	,	H	9	,	0										
370	,	 Building	,	250	,	 8{1}	,	H	1	,	0																													
380	,	 Building	,	250	,	 40{1}	,	H	4	,	90										
380	,	 Building	,	250	,	 80{1}	,	H	14	,	90																				
380	,	 Building	,	250	,	 125{1}	,	H	11	,	90										
380	,	 Building	,	250	,	 160{1}	,	H	4	,	90										
380	,	 Building	,	250	,	 195{1}	,	H	13	,	90										
380	,	 Building	,	250	,	 230{1}	,	H	23	,	90	
									
 /// 2 suburbs 420 to 580///						
 
430	,	 Building	,	250	,	 8{1}	,	H	14	,	0										
420	,	 Building	,	250	,	 40{1}	,	H	21	,	270										
420	,	 Building	,	250	,	 75{1}	,	H	4	,	270										
420	,	 Building	,	250	,	 105{1}	,	H	8	,	270										
420	,	 Building	,	250	,	 130{1}	,	H	15	,	270		
420	,	 Building	,	250	,	 170{1}	,	H	13	,	270		
420	,	 Building	,	250	,	 210{1}	,	H	9	,	270																			
455	,	 Building	,	250	,	 8{1}	,	H	16	,	0										
495	,	 Building	,	250	,	 8{1}	,	H	22	,	0										
625	,	 Building	,	250	,	 8{1}	,	H	3	,	0																			
675	,	 Building	,	250	,	 8{1}	,	H	19	,	0
680	,	 Building	,	250	,	 45{1}	,	H	12	,	90
680 ,	 Building	,	250	,	 80{1}	,	H	9	,	90
680	,	 Building	,	250	,	 125{1}	,	H	5	,	90
680	,	 Building	,	250	,	 175{1}	,	H	2	,	90
680	,	 Building	,	250	,	 215{1}	,	H	15	,	90

/// 3 suburbs 620 to 980 ///	
																				
733,	 Building	,	250	,	 8{1}	,	B	10	,	0
720 ,	 Building	,	250	,	 45{1}	,	B	7	,	270	
720 ,	 Building	,	250	,	 90{1}	,	B	6	,	270	
720 ,	 Building	,	250	,	 135{1}	,	H	14	,	270	
720 ,	 Building	,	250	,	 170{1}	,	H	12	,	270	
720 ,	 Building	,	250	,	 210{1}	,	H	20	,	270											
780 ,	 Building	,	250	,	 8{1}	,	H	19	,	0										
830 ,	 Building	,	250	,	 8{1}	,	H	18	,	0	
880 ,	 Building	,	250	,	 8{1}	,	H	17	,	0									0										
925 ,	 Building	,	250	,	 8{1}	,	B	3	,	0										
965 ,	 Building	,	250	,	 8{1}	,	B	11	,	0	
980 ,	 Building	,	250	,	 45{1}	,	B	5	,	90
980,	 Building	,	250	,	 85{1}	,	B	2	,	90
980,	 Building	,	250	,	 120{1}	,	H	9	,	90
980,	 Building	,	250	,	 175{1}	,	H	11	,	90
980,	 Building	,	250	,	 215{1}	,	H	2	,	90
									
/// 4 suburbs 1020 to 1280 ///			
																		
1038,	 Building	,	250	,	 8{1}	,	B	2	,	0	
1020,	 Building	,	250	,	 45{1}	,	B	10	,	270
1020,	 Building	,	250	,	 90{1}	,	B   8   ,	270
1020,	 Building	,	250	,	 115{1}	,	B	7	,	270									
1020,	 Building	,	250	,	 150{1}	,	B	9	,	270
1020,	 Building	,	250	,	 190{1}	,	H	2	,	270
1020,	 Building	,	250	,	 215{1}	,	H	1	,	270			
1080,	 Building	,	250	,	 8{1}	,	B	14	,	0	
1140,	 Building	,	250	,	 8{1}	,	B	16	,	0										
1210,	 Building	,	250	,	 8{1}	,	H	6	,	0										
1240,	 Building	,	250	,	 8{1}	,	H	16	,	0																			
1270,	 Building	,	250	,	 8{1}	,	B	6	,	0	
1280,	 Building	,	250	,	 45{1}	,	B	10	,	90	
1280,	 Building	,	250	,	 100{1}	,	B	2	,	90	
1280,	 Building	,	250	,	 135{1}	,	H	15	,	90	
1280,	 Building	,	250	,	 175{1}	,	H	19	,	90	
1280,	 Building	,	250	,	 210{1}	,	H	5	,	90	
									
///  5 Suburbs 1320 to 1430///				
																	
1335,	 Building	,	250	,	 8{1}	,	B	1	,	0
1320,	 Building	,	250	,	 60{1}	,	B	4	,	270
1320,	 Building	,	250	,	 95{1}	,	B	9	,	270
1320,	 Building	,	250	,	 130{1}	,	H	11	,	270	
1320,	 Building	,	250	,	 175{1}	,	H	18	,	270	
1320,	 Building	,	250	,	 220{1}	,	B	5	,	270								
1360,	 Building	,	250	,	 8{1}	,	B	7	,	0										
								

 //
 // Additional Objects e.g. trash bins, park benches
 //
 
 0, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 320, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 550, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 850, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%
 1220, Static Object, 150, -1.4{2}, 0, 270, 0, 0, %Bench%


 
 100, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 580, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 750, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 1020, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 1300, Static Object, 150, 1.4{1}, 0, 90, 0, 0, %Bench%
 
 //
 // Adding Curves
 //
 
 2100, Curve, 0, 75.8, 75.8, 75.8,  0.00300
 2400, Curve, 0, 75.8, 75.8, 75.8, -0.00200
 2700, Curve, 0, 75.8, 75.8, 75.8,  0.00100
 2950, Curve, 0, 75.8, 75.8, 75.8, -0.00250
 3300, Curve, 0, 75.8, 75.8, 75.8,  0.00440
