' Program:		Vortex Racing
' Objective:	Get to the shrinking circle or you will lose health. Collect as many circles as possible
' Programmer:	Mario D. Flores
' Date:			Wed, Dec 04, 2019

'*****************************************************************
' NOTES
Rem
	NOTE: "falling" from 575 pixels high, the object takes 546 ms to land
	at an acceleration of 3802 px/s^2 and a time unit of 0.001.
	That is an accurate calculation and should remain true regardless
	of how the code is changed.
End Rem

Rem
^(\t+)(?=Print)
Find Print commands
End Rem

'*****************************************************************
' DIRECTIVES
Strict

'*****************************************************************
' EXTERNAL LIBRAIRES
Import "libraries\basic_graphics\basic_graphics.bmx"
Import "libraries\shapes_f\shapes_f.bmx"
Import "libraries\math_structs\gfmath_lib.bmx"

'*****************************************************************
' GLOBAL CONSTANTS
'Mouse modes
Const MSE_LEFT = 1
Const MSE_RIGHT = 2
Const MSE_MIDDLE = 3

'8-directional movement constants
Const DPAD_LEFT = 1
Const DPAD_UP = 2
Const DPAD_RIGHT = 4
Const DPAD_DOWN = 8
Const DPAD_NW = DPAD_LEFT  | DPAD_UP
Const DPAD_NE = DPAD_RIGHT | DPAD_UP
Const DPAD_SE = DPAD_RIGHT | DPAD_DOWN
Const DPAD_SW = DPAD_LEFT  | DPAD_DOWN

'*****************************************************************
' TYPES
Type GameStats
	Field time:Float
	Field goals:Int, fails:Int
	Field collisions:Int
	Field goalCounter:Int
	Field failCounter:Int

	Field maxGoal:Int, maxFail:Int

	Function Create:GameStats()
		Local newStats:GameStats = New GameStats

		newStats.time = 0
		newStats.goals = 0
		newStats.fails = 0
		newStats.collisions = 0
		newStats.goalCounter = 0
		newStats.failCounter = 0

		Return newStats
	End Function

	Method UpdateTime(delta:Float)
		time :+ delta
	End Method

	Method CountCollision()
		collisions :+ 1
		maxFail = Max(maxFail, GetFailStreak())
		goalCounter = goals
	End Method

	Method CountGoal()
		goals :+ 1
		maxGoal = Max(maxGoal, GetGoalStreak())
		failCounter = fails
	End Method

	Method CountFail()
		fails :+ 1
		maxFail = Max(maxFail, GetFailStreak())
		goalCounter = goals
	End Method

	Method GetGoalStreak:Int()
		Return goals - goalCounter
	End Method

	Method GetFailStreak:Int()
		Return fails - failCounter
	End Method

	Method GetPeakGoals()
		Return maxGoal
	End Method

	Method GetPeakFails()
		Return maxFail
	End Method

	Method Reset()
		time = 0
		goals = 0
		fails = 0
		collisions = 0
		goalCounter = 0
		failCounter = 0
		maxGoal = 0
		maxFail = 0
	End Method

	Method PrintData()
		Print "Match time:     " + time
		Print "Goals:          " + goals
		Print "Misses:         " + fails
		Print "Collisions:     " + collisions
		Print "Goal streak     " + GetGoalStreak()
		Print "Fail streak     " + GetFailStreak()
		Print "Best streak:    " + GetPeakGoals()
		Print "Worst streak:   " + GetPeakFails()
	End Method
End Type

'*****************************************************************
' GLOBAL VARIABLES
'just generic loop variables
Local i:Int, j:Int

'size for game viewport
Global game_width, game_height
Global game_offset:Float2 = Float2.Create(0, 0)
Global hud_offset:Float2 = Float2.Create(0, 0)
Global hud_size:Float2 = Float2.Create(0, 0)

'*****************************************************************
' FUNCTIONS

'Returns a bitfield that uniquely represents a direction
Function Get8Dir(stateLeft, stateUp, stateRight, stateDown)
	Local retval:Int = 0
	If stateLeft Then	retval :| DPAD_LEFT
	If stateUp Then		retval :| DPAD_UP
	If stateRight Then	retval :| DPAD_RIGHT
	If stateDown Then	retval :| DPAD_DOWN

	Return retval
End Function

'*****************************************************************
' MAIN

'online applet to calculate Uniformly Accelerated Movement
'https://www.omnicalculator.com/physics/free-fall'

'Graphics Setup ----------------------
SetWindowed(800, 700)
'StartGraphics()

hud_size.Set(vp_width, 100)
game_width = vp_width
game_height = vp_height - hud_size.y
game_offset.Set(0, 0)

hud_offset.Set(0, game_height)

'Scene Setup -------------------------

SeedRnd(MilliSecs())

Const WALLS_COUNT = 11
Const FRAME_RATE = 60
Const UNIVERSAL_TIME_UNIT:Float = 0.001 'milliseconds to seconds (0.06 prev value)
Const SHIPS_MAX = 13
Const MAX_HP_GAIN = 2
Const MAX_HP_LOSS = 3
Const HP_MAX:Float = 1

Local screenDiag:Float = Sqr(game_width*game_width + game_height*game_height)
Local maxDamageDist:Float = screenDiag*0.75
Local match:GameStats = GameStats.Create()

'Ship properties
Local bgImage:TImage = LoadImage("images\dark_tile.jpg", 0)
Local rocketImage:TImage = LoadAnimImage("images\anim_spaceship.png", 64, 64, 0, 13)
Local thrustFire:TImage = LoadImage("images\thrust.png")
'Local shipImage:TImage = LoadAnimImage("images\spaceship.png", 32, 32, 0, 4)
Local shipDir:Float = 0, idxShip:Int = 0
Local frameFlipDelay:Float = 100 * UNIVERSAL_TIME_UNIT
Local animTimer:Float
Local health:Float = 1
Local hpRecoverRate:Float = 0.007
Local damageMult:Float = 1
Local levelDiff:Int = 0
Local rocketBBox:TOval = TOval.Create(0, 0, 7, 7)

'MidHandleImage(shipImage)
MidHandleImage(rocketImage)
MidHandleImage(thrustFire)

'Goal
Const GOALTIME_UNIT = 4000
Const GOALSIZE_BASE = 30
Local goalTimeLimit:Float = GOALTIME_UNIT * UNIVERSAL_TIME_UNIT
Local goalSize:Float = 30
Local goalTimer:Float
Local goal:TOval = TOval.Create(Rand(0, game_width - goalSize), Rand(0, game_height - goalSize), goalSize, goalSize)
Local failMaxDamage:Float = 0.5
Local failMinDamage:Float = 0.1
Local goalReward:Float = 0.02
Local goalPoints:Int = 0
Local gainMultiplier:Int

'FPS/timing variables
Local renderTime:Int
Local frameStamp:Int
Local frameDelta:Int
Local frameRateDelay:Float = 1000.0 / FRAME_RATE
Local temporalScalar:Float = 0
Local partialTime:Float
Local timeAcc:Float
Local timeDist:Float
Local timeVel:Float

'objects
Local walls:TLine[WALLS_COUNT]
Local wallsUbound = Len(walls) - 1

'Collision properties
Local timeVector:TLine = TLine.Create(0, 0, 0, 0)
Local deflectVector:Float2 = Float2.Create(0, 0)
Local deflectAngle:Float
Local collPos:Float2, collCurr:Float2
Local collIndex:Int
Local normalMatch:Int, normalMatchAhead:Int
Local normalCollision:Int
Local normalForce:Float2
Local collSpeed:Float2

'Movement properties
Local p1_pos:Float2 = Float2.Create(game_width * 0.875, game_height * 0.0417)
Local p1_prevSpeed:Float2 = Float2.Create(0, 0)
Local p1_speed:Float2 = Float2.Create(0, 0)
Local acceleration:Float2 = Float2.Create(0, 0)
Local gravity:Float2 = Float2.Create(0, 0)'1267) '(0, 0.98)
Local speedLimit:Float = 420 '17
Local speedDecay:Float = 0.2 '0.1 '0.20
Local thrustPower:Float = 750 '3

'Sounds
Local explosionSound:TSound = LoadSound("sounds\big_crash.wav", 0)
Local explosionChannel:TChannel
Local bounceSound:TSound = LoadSound("sounds\small_crash.wav", 0)
Local bounceChannel:TChannel
Local pauseMusic:TSound = LoadSound("sounds\01_looped_piano_mel.wav", SOUND_LOOP)
Local pauseMChannel:TChannel
Local clickSound:TSound = LoadSound("sounds\synth_chord_d-fs.wav", 0)
Local clickChannel:TChannel
Local musicSound:TSound = LoadSound("sounds\02_bass_battery.ogg", SOUND_LOOP)
Local musicChannel:TChannel

'Timing properties
Local bounceVolume:Float
Local soundTimer:Float, timerDelta:Float
Local countLowDeltas:Int
Local afterDeathTimer:Float
Const GAME_OVER_GRACE:Float = 2500 * UNIVERSAL_TIME_UNIT '1000 time units - milliseconds are default so five second

'gamepad input variables
Local joyNum = JoyCount()
Local joyIndex = -1
Local jyName:String
Local joyVector:Float2 = Float2.Create(0, 0)

'GUI objects
Const GUI_OBJ_GAP = 4
Local barLength:Int = 240
Local barHeight:Int = 20
Local hpBackg:TRect = TRect.Create((game_width - barLength)/2, hud_offset.y + (hud_size.y - barHeight)/3, barLength, barHeight)
Local hpBar:TRect = TRect.Create(hpBackg.location.x + 2, hpBackg.location.y + 2, hpBackg.size.x - 4, hpBackg.size.y - 4)

'Misc.
Local mseLoc:Float2 = Float2.Create(0, 0)
Local frozenImage:TImage = CreateImage(vp_width, vp_height)

'vf = Sqr(2*a*d + vi^2)
'Calculates max possible speed in current viewport (for sound effect purposes)
Local maxSpeed:Float = Sqr(2*gravity.y*game_height + speedLimit*speedLimit)

'link player location to bound circle location
rocketBBox.location = p1_pos

'Determines the number of gaming input devices available
If joyNum > 0 Then
	joyIndex = 0
	jyName = JoyName(joyIndex)
End If

'Set font style
SetImageFont(LoadImageFont("images\consola.ttf", 24))

'-----------------------------------------------------
'SOUND PRE-LOADING

'prepares the sound for the first play
'for some reason this creates a little delay
bounceChannel = CueSound(bounceSound)
SetChannelVolume(bounceChannel, 0)
ResumeChannel(bounceChannel)
explosionChannel = CueSound(explosionSound)
SetChannelVolume(explosionChannel, 0)
ResumeChannel(explosionChannel)

pauseMChannel = CueSound(pauseMusic)
SetChannelVolume(pauseMChannel, 0)
ResumeChannel(pauseMChannel)
PauseChannel(pauseMChannel)
SetChannelVolume(pauseMChannel, 0.4)

clickChannel = CueSound(clickSound)
SetChannelVolume(clickChannel, 0)
ResumeChannel(clickChannel)

musicChannel = CueSound(musicSound)
SetChannelVolume(musicChannel, 0)
ResumeChannel(musicChannel)
PauseChannel(musicChannel)
SetChannelVolume(musicChannel, 0.4)

'-----------------------------------------------------
'SCENE SETUP

'Left
walls[0] = TLine.Create(0, 0, 0, game_height)
walls[0].normal = 0
'Top
walls[1] = TLine.Create(0, 0, game_width, 0)
walls[1].normal = 1
'Right
walls[2] = TLine.Create(game_width - 1, 0, game_width - 1, game_height)
walls[2].normal = 1
'Bottom
walls[3] = TLine.Create(0, game_height - 1, game_width, game_height - 1)
walls[3].normal = 0

'Corner top left
walls[4] = TLine.Create(0, 25, 25, 0)
walls[4].normal = 1
'Corner top right
walls[5] = TLine.Create(game_width - 25, 0, game_width, 25)
walls[5].normal = 1
'Corner bottom left
walls[6] = TLine.Create(0, game_height - 25, 25, game_height)
walls[6].normal = 0
'Corner bottom right
walls[7] = TLine.Create(game_width - 25, game_height, game_width, game_height - 25)
walls[7].normal = 0


'Ramp 1 - Lower
walls[8] = TLine.Create(game_width * 0.22, game_height * 0.85, game_width * 0.9, game_height * 0.72)
walls[8].Scale(0.9, 1)
walls[8].normal = 0

'Ramp 2 - Middle
walls[9] = TLine.Create(game_width * 0.1, game_height * 0.43, game_width * 0.78, game_height * 0.57)
walls[9].Scale(0.82, 0)
walls[9].normal = 0

'Ramp 3 - Upper
walls[10] = TLine.Create(game_width * 0.22, game_height * 0.29, game_width * 0.9, game_height * 0.14)
walls[10].Scale(0.85, 1)
walls[10].normal = 0
'-----------------------------------------------------

soundTimer = MilliSecs()
Local timer:Int = MilliSecs()
Local showHud:Int = True
Local drawPath:Int = False

SetLineWidth(2)
SetBlend(ALPHABLEND)

'Start playing background music
ResumeChannel(musicChannel)
'Sets first timer
goalTimeLimit = (p1_pos.GetDistance(goal.location)/screenDiag * GOALTIME_UNIT + GOALTIME_UNIT - MathAlg.Qerp(0,0,1,Min(Float(levelDiff)/50,1))*GOALTIME_UNIT) * UNIVERSAL_TIME_UNIT

'Main Loop ---------------------------
Repeat
	'PRE-FRAME SETUP ================================================================
	frameStamp = MilliSecs()
	SetColor(255, 255, 255)
	match.UpdateTime(temporalScalar)

	If health < 0 Then
		If afterDeathTimer = 0 Then
			explosionChannel = CueSound(explosionSound)
			SetChannelVolume(explosionChannel, 0.75)
			PauseChannel(musicChannel)
			ResumeChannel(explosionChannel)
		End If
		afterDeathTimer :+ temporalScalar
	End If

	Select True
		Case KeyHit(KEY_BACKSPACE) Or JoyHit(6)
			'Resets the game to initial state
			p1_pos.Set(game_width * 0.875, game_height * 0.0417)
			p1_speed.Set(0, 0)
			Cls
			TileImage(bgImage)
			goalTimer = 0
			health = 1
			gainMultiplier = 0
			'goalPoints = 0
			match.Reset()
			goal.location.x = Rand(0, game_width - goalSize)
			goal.location.y = Rand(0, game_height - goalSize)
			goalTimeLimit = (p1_pos.GetDistance(goal.location)/screenDiag * GOALTIME_UNIT + GOALTIME_UNIT - MathAlg.Qerp(0,0,1,Min(Float(levelDiff)/50,1))*GOALTIME_UNIT) * UNIVERSAL_TIME_UNIT

		'Case KeyHit(KEY_H) Or JoyHit(5)
			'showHud = Not showHud

		'Case KeyHit(KEY_T) Or JoyHit(0)
			'drawPath = Not drawPath

		Case KeyHit(KEY_ESCAPE) Or JoyHit(7)
			GrabImage(frozenImage, 0, 0)
			PauseChannel(musicChannel)
			ResumeChannel(pauseMChannel)

			Repeat
				'This line here may be dangerous if you have assets that need to be
				'unloaded before exiting. Update properly as project goes on.
				SetAlpha(1)
				DrawImage(frozenImage, 0, 0)
				
				SetColor(250, 250, 250)
				DrawText("Written by Mario D Flores", 0, 0)
				SetAlpha((Sin(MilliSecs()*0.25) + 1)*0.4 + 0.2)
				SetColor(0, 200, 250)
				DrawText("PAUSE", (game_width - TextWidth("PAUSE"))/2, (game_height - TextHeight("P"))/2)

				Flip
				If JoyHit(6) Or AppTerminate() Or KeyHit(KEY_BACKSPACE) Then
					match.PrintData()
					End
				End If
			Until KeyHit(KEY_ESCAPE) Or JoyHit(7)
			SetAlpha(1)

			PauseChannel(pauseMChannel)
			ResumeChannel(musicChannel)
			SetColor(255, 255, 255)
			frameStamp = MilliSecs()

	End Select

	If Not drawPath Then
		Cls 'Clear backbuffer
		TileImage(bgImage)
	End If

	'Reset acceleration
	acceleration.Set(0, 0)

	'Recover health over time
	If health > 0 Then health = Min(health + temporalScalar * hpRecoverRate * Min(Max(gainMultiplier, -MAX_HP_LOSS), MAX_HP_GAIN), HP_MAX)

	'increases goal timer
	goalTimer :+ temporalScalar

	'caught goal on time
	If goal.IsCircleBound(rocketBBox) And health > 0 Then
	'If goal.IsPointInside(p1_pos) And health > 0 Then
		'adds one to health gain multiplier (positive)
		If gainMultiplier < 0 Then
			gainMultiplier = 0
		Else
			gainMultiplier = gainMultiplier + 1
		End If
		'regenerates some health
		health :+ goalReward * Min(gainMultiplier, MAX_HP_GAIN)
		'caps health to 1
		health = Min(health, 1)
		'adds one goal point to total
		'goalPoints :+ 1
		match.CountGoal()

		'moves goal to a random location
		goal.location.x = Rand(0, game_width - goalSize)
		goal.location.y = Rand(0, game_height - goalSize)

		'updates level of game
		levelDiff = Floor(match.goals * 0.1)
		'changes goal size according to level
		goalSize = 30 - Min(levelDiff, 10)
		'increases damage taken according to level
		damageMult = 1 + Min(levelDiff, 8)*0.25

		'resets timer to zero
		goalTimer = 0
		'updates time limit based on distance to goal
		'goalTimeLimit = (p1_pos.GetDistance(goal.location)/screenDiag * GOALTIME_UNIT + GOALTIME_UNIT) * UNIVERSAL_TIME_UNIT
		goalTimeLimit = (p1_pos.GetDistance(goal.location)/screenDiag * GOALTIME_UNIT + GOALTIME_UNIT - MathAlg.Qerp(0,0,1,Min(Float(levelDiff)/20,1))*GOALTIME_UNIT) * UNIVERSAL_TIME_UNIT

		'play goal sound
		clickChannel = CueSound(clickSound)
		SetChannelVolume(clickChannel, 0.4)
		ResumeChannel(clickChannel)
	End If

	'goal imploded you lose health
	If goalTimer > goalTimeLimit And health > 0 Then
		'adds one to health loss multiplier (negative)
		gainMultiplier = Min(gainMultiplier, 0) - 1
		'degrades health based on distance to goal
		health :- Max(Min(p1_pos.GetDistance(goal.location), maxDamageDist)/maxDamageDist * failMaxDamage, failMinDamage) * Min(Abs(gainMultiplier), MAX_HP_LOSS)
		match.CountFail()

		'moves goal to a random location
		goal.location.x = Rand(0, game_width - goalSize)
		goal.location.y = Rand(0, game_height - goalSize)
		'goalSize = 30

		'resets timer but adds remainder past the limit
		goalTimer = goalTimer Mod temporalScalar
		'updates time limit based on distance to goal
		goalTimeLimit = (p1_pos.GetDistance(goal.location)/screenDiag * GOALTIME_UNIT + GOALTIME_UNIT - MathAlg.Qerp(0,0,1,Min(Float(levelDiff)/20,1))*GOALTIME_UNIT) * UNIVERSAL_TIME_UNIT
	End If

	'resizes goal based on level and current timer
	goal.size.x = goalSize * (1 - goalTimer/goalTimeLimit)
	goal.size.y = goalSize * (1 - goalTimer/goalTimeLimit)

	'USER INPUT =====================================================================

	'WASD keyboard input (8-directional)
	Select Get8Dir(KeyDown(KEY_A), KeyDown(KEY_W), KeyDown(KEY_D), KeyDown(KEY_S))
		Case DPAD_LEFT
			acceleration.Set(-thrustPower, 0)
		Case DPAD_UP
			acceleration.Set(0, -thrustPower)
		Case DPAD_RIGHT
			acceleration.Set(thrustPower, 0)
		Case DPAD_DOWN
			acceleration.Set(0, thrustPower)
		Case DPAD_NW
			acceleration.SetPolar(thrustPower, -135)
		Case DPAD_NE
			acceleration.SetPolar(thrustPower, -45)
		Case DPAD_SE
			acceleration.SetPolar(thrustPower, 45)
		Case DPAD_SW
			acceleration.SetPolar(thrustPower, 135)
	End Select

	'Joystick input
	If joyNum > 0 Then
		'Creates a vector with the analogue stick input. Deadzone of +-0.15 to +-1
		joyVector.SetWPLimit(JoyX(joyIndex) * thrustPower, JoyY(joyIndex) * thrustPower, thrustPower*0.15, thrustPower)

		If joyVector.x <> 0 Or joyVector.y <> 0 Then
			acceleration.Set(joyVector.x, joyVector.y)
		End If
	End If

	'Changes the current used gaming input device
	If KeyHit(KEY_PAGEUP) And joyNum > 0 Then
		joyIndex = (joyIndex + 1) Mod joyNum
		jyName = JoyName(joyIndex)
	End If

	'If (500 - p1_pos.x) <> 0 Then
		'This code will generate a gravity well at (500, 400)
		'gravity.SetPolar(30000/Sqr((400 - p1_pos.y)*(400 - p1_pos.y) + (500 - p1_pos.x)*(500 - p1_pos.x)), ATan2(400 - p1_pos.y, 500 - p1_pos.x))
	'End If

	'PHYSICS AND COLLISIONS =========================================================
	If health < 0 Then
		'Reset acceleration
		acceleration.Set(0, 0)
		'Slow down rocket
		If p1_speed.GetMagnitude() > 0 Then
			p1_speed.SetMagnitude(Max(p1_speed.GetMagnitude() - 200 * temporalScalar, 0))
		End If
	End If

	'saves previous speed
	p1_prevSpeed.y = p1_speed.y

	'Stores the current location (tail) and predicted path (head) of the object
	timeVector.p2.Set(p1_pos.x, p1_pos.y)
	timeVector.p2.IncreaseRet(p1_speed.x * temporalScalar + (acceleration.x + gravity.x)*temporalScalar*temporalScalar*0.5, p1_speed.y * temporalScalar + (acceleration.y + gravity.y)*temporalScalar*temporalScalar*0.5, timeVector.p1)

	collPos = Null
	'Determine if a collision is going to happen on next update
	For i = 0 To wallsUbound

		Rem
			REFACTOR: RIGHT NOW THE COLLISION SYSTEM IS IMPLEMENTED DETECTING AN INTERSECTION BETWEEN
			A LINEAR PREDICTED PATH OF THE BALL AND THE COLLIDER (WHICH IS ALSO A LINE). HOWEVER THIS
			IS SLIGHTLY INACCURATE BECAUSE THE PATH TRACED BY A UNIFORMLY* ACCELERATED PATH IS QUADRATIC.
			THEREFORE A MORE FAITHFULL REPRESENTATION WOULD BE TO USE A LINE TO QUADRATIC BEZIER INTERSECTION.

			*UNIFORM WITHIN A FRAME. THE SPEED IS TECHNICALLY ACCELERATED TOO (JOLT/JERK), BUT ACCOUNTING FOR
			RANDOM VARIATIONS IN ACCELRATION IS A POLYNOMIAL OF VARYING DEGREES. LINEAR IS ACTUALLY
			A GOOD COMPROMIZE BUT A QUADRATIC APROXIMATION WOULD DO A BETTER JOB
		End Rem

		'returns a Float2 structure if there is an intersection, null otherwise
		collCurr = FindIntersectionSafe(timeVector, walls[i], 0.025)

		'Enters only when there was a collision detected with current object
		If collCurr <> Null Then
			If collPos = Null Then
				'if there hasn't been a collision yet then just assign it
				collPos = collCurr
				collIndex = i
			Else
				'if there has been a collision update only if new collision is closer to point
				If timeVector.p1.GetDistance(collCurr) < timeVector.p1.GetDistance(collPos) Then
					collPos = collCurr
					collIndex = i
				End If
			End If
		End If
	Next

	'Determines which side of the normal relative to the collider the current and predicted paths are
	normalMatch = walls[collIndex].IsOnNormal(timeVector.p1)
	normalMatchAhead = walls[collIndex].IsOnNormal(timeVector.p2)

	'A mismatch of normals means there is a collision. True when going through facing the normal
	'remove true and uncomment code for directional collision detection
	normalCollision = True '(normalMatch = True) And (normalMatchAhead = False)

	'Enters if there is a collision and the object is going through facing the normal of the collider
	If collPos <> Null And normalCollision Then
		'backtrack location
		timeVector.p2.Set(timeVector.p1.x, timeVector.p1.y)

		't = (-vi +- sqrt(vi^2 + 2*a*d))/a
		'coefficients of time calculation
		timeAcc= acceleration.Add(gravity).GetMagnitude()
		timeDist= collPos.GetDistance(timeVector.p1)
		timeVel= p1_speed.GetMagnitude()

		'if timeAcc is too small (zero +- rounding error) the problem becomes linear. A quadratic solution would be undefined.
		If timeAcc > 0.00001 Then
			'time past until collision
			partialTime = (-timeVel + Sqr(timeVel*timeVel + 2*timeAcc*timeDist))/timeAcc
		Else
			partialTime = timeDist/timeVel
		End If

		'flatten out rounding errors
		If partialTime > temporalScalar Then partialTime = temporalScalar

		'updates speed with partial time delta
		p1_speed.IncreaseWPLimit(acceleration.x * partialTime, acceleration.y * partialTime, Max(speedLimit, p1_speed.GetMagnitude()))
		p1_speed.Increase(gravity.x * partialTime, gravity.y * partialTime)

		'calculates the instantaneous collision-speed using vector projection
		collSpeed = GetProjection(p1_speed, walls[collIndex].GetNormal())

		'user collision speed to deduct health
		health :- collSpeed.GetMagnitude()/(maxSpeed * 4) * damageMult
		'calculates volume of collision sound effect
		bounceVolume = Abs(collSpeed.GetMagnitude()/maxSpeed)

		'updates location of estimated next location with partial time
		'timeVector.p2.Move(p1_speed.x * partialTime + (acceleration.x + gravity.x)*partialTime*partialTime, p1_speed.y * partialTime + (acceleration.y + gravity.y)*partialTime*partialTime)
		'THIS IS A WORKAROUND TO A BUG. WILL BE UPDATED EVENTUALLY
		timeVector.p2.Set(collPos.x, collPos.y)

		'gets a unit unit vector equal to the normal of colliding surface (direction doesn't matter here)
		normalForce = walls[collIndex].GetNormal()
		'additive inverse of the unit vector minus some decay
		normalForce.x = (1 - Abs(normalForce.x) * speedDecay)
		normalForce.y = (1 - Abs(normalForce.y) * speedDecay)

		'Calculates the angle of deflection (bounce)
		deflectAngle = walls[collIndex].GetAngle() * 2 - p1_speed.GetAngle()

		'makes speed deflect
		p1_speed.SetPolar(p1_speed.GetMagnitude(), deflectAngle)

		'makes speed decay component-wise
		p1_speed.x :* normalForce.x
		p1_speed.y :* normalForce.y

		'backtrack location
		'timeVector.p2.Set(timeVector.p1.x, timeVector.p1.y)
		'timeVector.p2.Set(collPos.x, collPos.y)

		'update predicted path (head) with lost travel in the right direction
		'timeVector.p2.Move(p1_speed.x * partialTime + (acceleration.x + gravity.x)*partialTime*partialTime, p1_speed.y * partialTime + (acceleration.y + gravity.y)*partialTime*partialTime)

		'calculates rest of time after collision
		partialTime = temporalScalar - partialTime

		Rem
			TODO: I need to find a way to add the rest of the travel after the collision regression without tresspassing the collision boundaries.
			Scenario one is that the ball bounces away with enough force to overcome acceleration. Scenario two is the ball does not have enough
			force to overcome acceleration and moves past the collider thus bypassing collision detection.
			CAUTION: Adding recursion to try to fix it would cause an infinite loop.

			IDEA: A normal check might work. This would be a method which tells if the speed is going towards or away from the surface's normal. If it is
			going towards the surface then use the normal force to truncate the acceleration trying to bypass the collider. This might also allow
			to control friction. SEE ALSO VECTOR PROJECTION.
		End Rem

		'updates speed with remainder time delta
		'p1_speed.IncreaseWPLimit(acceleration.x * partialTime, acceleration.y * partialTime, Max(speedLimit, p1_speed.GetMagnitude()))
		'p1_speed.Increase(gravity.x * partialTime, gravity.y * partialTime)

		'updates location of estimated next location with remainder time delta
		'timeVector.p2.Move(p1_speed.x * partialTime, p1_speed.y * partialTime)

		'update previous path (tail)
		'timeVector.p1.Set(collPos.x, collPos.y)

		'calculates the time since the last collision
		timerDelta = MilliSecs() - soundTimer

		'if the time since the last collision is less than 48 ms
		'accumulate counter to stop playing the sound effect
		If timerDelta < 48 Then
			countLowDeltas :+ 1
		Else
			'reset to zero after a period longer than 48 ms
			countLowDeltas = 0
		End If

		'prevent more than 5 consecutive clicks when colliding
		If countLowDeltas < 5 Then
			bounceChannel = CueSound(bounceSound)
			SetChannelVolume(bounceChannel, bounceVolume*0.5 + 0.25)
			ResumeChannel(bounceChannel)
		End If
		soundTimer = MilliSecs()

		match.CountCollision()
		If gainMultiplier > 0 Then gainMultiplier = 0
	Else
		'in this branch of if-statement no collision happened

		'updates with user-input acceleration
		p1_speed.IncreaseWPLimit(acceleration.x * temporalScalar, acceleration.y * temporalScalar, Max(speedLimit, p1_speed.GetMagnitude()))
		'updates with gravity
		p1_speed.Increase(gravity.x * temporalScalar, gravity.y * temporalScalar)
	End If

	'object location updated
	p1_pos.Set(timeVector.p2.x, timeVector.p2.y)

	'DRAW AND GRAPHICS ==============================================================

	'Animations
	'start death animation when health drops to 0
	If health <= 0 And idxShip < (SHIPS_MAX - 1) Then
		animTimer :+ temporalScalar
		If animTimer > frameFlipDelay Then
			idxShip = (idxShip + 1) Mod SHIPS_MAX
			animTimer = animTimer Mod frameFlipDelay
		End If
	Else If health > 0 Then
		'Keep timer at 0 while rocket is alive
		animTimer = 0
		idxShip = 0
	End If

	'SetColor(255, 0, 0)
	'timeVector.Draw()

	'Draw walls
	SetColor(128, 192, 255)
	For i = 0 To wallsUbound
		walls[i].Draw()
	Next

	'calculate direction of rocket
	If p1_speed.GetMagnitude() > 0 Then shipDir = p1_speed.GetAngle()
	'Draw ship with appropriate transformation
	SetColor(255, 255, 255)
	SetTransform(shipDir + 90, 0.5, 0.5)
	DrawImage(rocketImage, p1_pos.x, p1_pos.y, idxShip)

	'Draw thrust when thrust is being applied
	If acceleration.GetMagnitude() > 0 Then
		DrawImage(thrustFire, p1_pos.x, p1_pos.y)
	End If

	'Reset transform to default
	SetTransform()

	'Draw goal with 30% transparency
	SetAlpha(0.7)
	SetColor(148,196,255)
	goal.Draw()

	'Draw hud
	If showHud And Not drawPath Then
		SetAlpha(1)
		SetColor(48, 54, 64)
		DrawRect(hud_offset.x, hud_offset.y, hud_size.x, hud_size.y)
		SetColor(255, 255, 255)
		DrawText("Pts: " + match.goals, hud_offset.x + hpBackg.location.x/2 - 42, hpBackg.location.y)
		DrawText("Level: " + levelDiff, hud_size.x - hpBackg.location.x/2 - 60, hpBackg.location.y)
		If gainMultiplier > 0 Then
			SetColor(10, 220, 10)
		Else If gainMultiplier < 0 Then
			SetColor(220, 10, 10)
		Else
			SetColor(48, 54, 64)
		End If
		DrawText("x" + Abs(Min(Max(gainMultiplier, -MAX_HP_LOSS), MAX_HP_GAIN)), hud_size.x/2 - 20, hpBar.location.y + hpBar.size.y + 8)

		'Draw health bar
		hpBar.size.x = Max(health, 0) *  (hpBackg.size.x - GUI_OBJ_GAP)
		SetAlpha(0.5)
		SetColor(0, 0, 0)
		hpBackg.Draw()
		SetAlpha(0.8)
		SetColor(64, 192, 64)
		hpBar.Draw()
		SetAlpha(1)

		If health <= 0 Then
			SetColor(255, 0, 0)
			DrawText("GAME OVER", (game_width - TextWidth("GAME OVER"))/2, (game_height - TextHeight("G"))/2)
		End If
	End If

	'Flip backbuffer to front
	Flip

	'FRAME RATE =====================================================================

	'calculates time to render frame
	renderTime = MilliSecs() - frameStamp

	'select if FPS is fast or slow
	If renderTime > frameRateDelay Then
		'frame rate is dropping below threshold
		frameDelta = renderTime
	Else
		'frame rate too fast add delay
		Delay (frameRateDelay - renderTime)
		frameDelta = MilliSecs() - frameStamp
	End If

	'calculates total time elapsed per frame
	temporalScalar = UNIVERSAL_TIME_UNIT * frameDelta

Until AppTerminate() Or (health < 0 And afterDeathTimer > GAME_OVER_GRACE) 'KeyHit(KEY_ESCAPE) Or

match.PrintData()
End
'*****************************************************************
