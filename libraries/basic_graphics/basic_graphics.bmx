' Module:		Basic MAX Graphics
' Programmer:	Mario D. Flores (aka SlimRunner)

'*****************************************************************
' DIRECTIVES
Strict
'pre-declared for for-loops
Local i:Int

'*****************************************************************
' NOTES
' This library starts a basic graphic device to do quick-n-dirty
' graphic applications.

'*****************************************************************
' EXTERNAL LIBRAIRES
' No Dependencies

'*****************************************************************
' GLOBAL CONSTANTS
Const MIN_WIDTH = 100
Const MIN_HEIGHT = 100

'Color modes
Const BPP_AUTO = 0  'unspecified color mode, windowed
Const BPP_HALF = 16 '16-bit color mode, fullscreen
Const BPP_FULL = 24 '24-bit color mode, fullscreen
Const BPP_EXTD = 32 '32-bit color mode, fullscreen

'Symbol used as separator for width and height
Const RES_SYMBOL:String = "x"

'*****************************************************************
' TYPES
'No types here

'*****************************************************************
' GLOBAL VARIABLES
'View-port dimensions
Global vp_width = 640 'default
Global vp_height = 480 'default

Global max_width
Global max_height

'*****************************************************************
' FUNCTIONS
Function ParseResolution:Int[](res:String, symbol:String)
	Local retval:Int[2]

	symbol = Left(symbol, 1)

	Local inputLen = Len(res)
	Local signLoc:Int = Instr(res, symbol)

	retVal[0] = Int(Left(res, signLoc - 1))
	retVal[1] = Int(Right(res, inputLen - signLoc))

	Return retVal
End Function

Function StartGraphics()
	Print
	Print "Loading graphic modes..."

	Local i:Int 'pre-declared for loops
	Local tg_modes:TGraphicsMode[] = GraphicsModes()
	Local tg_ubound = CountGraphicsModes()-1

	max_width = tg_modes[tg_ubound].width
	max_height = tg_modes[tg_ubound].height

	Print (tg_ubound + 1) + " graphic modes have been found."
	Print
	Print "0: None, windowed mode"

	For i = 0 To tg_ubound
		Print (i + 1) + ": " + tg_modes[i].width + "x" + tg_modes[i].height + "px, " + tg_modes[i].depth + "bpp, " + tg_modes[i].hertz + "hz"
	Next

	Local tg_user_pick:String = Input("Pick a graphic mode: ")
	Local tg_user_cast:Int = Int(tg_user_pick) - 1

	If tg_user_cast < 0 Then
		'Use windowed mode
		Print ""
		Print "Windowed Mode (empty for default)"
		tg_user_pick = Input("Enter resolution [WxH]: ")
		Local user_res:Int[]

		If tg_user_pick <> "" Then
			user_res = ParseResolution(tg_user_pick, RES_SYMBOL)
			While (user_res[0] < MIN_WIDTH Or user_res[0] > max_width Or user_res[1] < MIN_HEIGHT Or user_res[1] > max_height) And tg_user_pick <> ""
				Print "Resolution is not within valid range"
				Print MIN_WIDTH + " < W < " + max_width + " , " + MIN_HEIGHT + " < H < " + max_height
				tg_user_pick = Input("Enter resolution [WxH]: ")
				user_res = ParseResolution(tg_user_pick, RES_SYMBOL)
			Wend

			If tg_user_pick <> "" Then
				vp_width = user_res[0]
				vp_height = user_res[1]
			End If
		End If

		Graphics vp_width, vp_height, BPP_AUTO
		Print
		Print "Windowed mode"
		Return True

	Else If tg_user_cast <= tg_ubound Then
		'Use input as index (input is already offset)
		vp_width = tg_modes[tg_user_cast].width
		vp_height = tg_modes[tg_user_cast].height

		Graphics vp_width, vp_height, tg_modes[tg_user_cast].depth, tg_modes[tg_user_cast].hertz
		Print
		Print "Fullscreen mode"
		Return True

	Else
		Return False

	End If
End Function

Function SetWindowed(width:int, height:Int)
	Print
	Print "Loading graphic modes..."

	Local tg_modes:TGraphicsMode[] = GraphicsModes()
	Local tg_ubound = CountGraphicsModes()-1

	max_width = tg_modes[tg_ubound].width
	max_height = tg_modes[tg_ubound].height

	Local param_res:Int[2]

		param_res[0] = width
		param_res[1] = height

	If (param_res[0] < MIN_WIDTH Or param_res[0] > max_width Or param_res[1] < MIN_HEIGHT Or param_res[1] > max_height) Then
		Print "Error Loading Graphics"
		Return False
	End If

	vp_width = param_res[0]
	vp_height = param_res[1]

	Graphics vp_width, vp_height, BPP_AUTO
	Print
	Print "Windowed mode " + vp_width + "x" + vp_height
	Return True
End Function

Function SetWindowedText(param:String)
	Print
	Print "Loading graphic modes..."

	Local tg_modes:TGraphicsMode[] = GraphicsModes()
	Local tg_ubound = CountGraphicsModes()-1

	max_width = tg_modes[tg_ubound].width
	max_height = tg_modes[tg_ubound].height

	Local param_res:Int[]

	If param <> "" Then
		param_res = ParseResolution(param, RES_SYMBOL)
		If (param_res[0] < MIN_WIDTH Or param_res[0] > max_width Or param_res[1] < MIN_HEIGHT Or param_res[1] > max_height) And param <> "" Then
			Return False
		End If

		If param <> "" Then
			vp_width = param_res[0]
			vp_height = param_res[1]
		End If
	End If

	Graphics vp_width, vp_height, BPP_AUTO
	Print
	Print "Windowed mode " + vp_width + "x" + vp_height
	Return True
End Function

'*****************************************************************
' INITIALIZATION
' Nothing to do here
