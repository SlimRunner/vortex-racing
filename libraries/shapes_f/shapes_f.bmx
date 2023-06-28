' Module:		Basic classes to manage shapes
' Programmer:	Mario D. Flores (aka SlimRunner)

'*****************************************************************
' DIRECTIVES
Strict

'*****************************************************************
' NOTES


'*****************************************************************
' EXTERNAL LIBRAIRES
Import "..\math_structs\gfmath_lib.bmx"

'*****************************************************************
' GLOBAL CONSTANTS
'pre-declared for for-loops
Local i:Int

'*****************************************************************
' TYPES

'Deprecated type
Type TPoint
	Field x:Float, y:Float

	Function Create:TPoint(x_loc:Float, y_loc:Float)
		Local newPoint:TPoint = New TPoint
		newPoint.x = x_loc
		newPoint.y = y_loc

		Return newPoint
	End Function

	Method SetPos(x_loc:Float, y_loc:Float)
		x = x_loc
		y = y_loc
	End Method

	Method Move(x_loc:Float, y_loc:Float)
		x :+ x_loc
		y :+ y_loc
	End Method

	Method Move2(x_loc:Float, y_loc:Float, prevLoc:TPoint)
		prevLoc.SetPos(x, y)
		x :+ x_loc
		y :+ y_loc
	End Method

	Method MoveToMouse()
		x = MouseX()
		y = MouseY()
	End Method

	Method MoveToMouse2(delta:TPoint)
		delta.x = -x
		delta.y = -y

		x = MouseX()
		y = MouseY()

		delta.x :+ x
		delta.y :+ y
	End Method

	Method FindDistance:Float(other:TPoint)
		Local xDelta:Float = x - other.x
		Local yDelta:Float = y - other.y

		Return Sqr(xDelta*xDelta + yDelta*yDelta)
	End Method
End Type

Type TLine
	Field p1:Float2, p2:Float2
	Field normal:Int 'true/false'

	Function Create:TLine(x1:Float, y1:Float, x2:Float, y2:Float)
		Local newLine:TLine = New TLine
		newLine.p1 = Float2.Create(x1, y1)
		newLine.p2 = Float2.Create(x2, y2)

		Return newLine
	End Function

	'Moves starting point to where the mouse is
	Method MoveStartToMouse()
		p1.x = MouseX()
		p1.y = MouseY()
	End Method

	'Moves ending point to where the mouse is
	Method MoveEndToMouse()
		p2.x = MouseX()
		p2.y = MouseY()
	End Method

	Method Move(x_loc:Float, y_loc:Float)
		p1.x :+ x_loc
		p1.y :+ y_loc
		p2.x :+ x_loc
		p2.y :+ y_loc
	End Method

	Method Draw()
		DrawLine(Fix(p1.x), Fix(p1.y), Fix(p2.x), Fix(p2.y))
	End Method

	Method GetXDelta:Float()
		Return p2.x - p1.x
	End Method

	Method GetYDelta:Float()
		Return p2.y - p1.y
	End Method

	Method GetXSlope:Float()
		Return Double(p2.y - p1.y)/(p2.x - p1.x)
	End Method

	Method GetYSlope:Float()
		Return Double(p2.x - p1.x)/(p2.y - p1.y)
	End Method

	Method GetSlope:Float()
		Local dx:Float = p2.x - p1.x
		Local dy:Float = p2.y - p1.y

		If Abs(dx) > Abs(dy) Then
			'slope of x
		 	Return dy/dx
		Else
			'slope of y
			Return dx/dy
		End If
	End Method

	Method GetMagnitude()
		Local dx:Float = p2.x - p1.x
		Local dy:Float = p2.y - p1.y

		Return Sqr(dx*dx + dy*dy)
	End Method

	Method GetAngle:Float()
		Return ATan2(p2.y - p1.y, p2.x - p1.x)
	End Method

	Method GetUnitVector:Float2()
		Local unitVector:Float2 = Float2.Create(GetXDelta(), GetYDelta())
		unitVector.Normalize()

		If Not normal Then
			unitVector.x :* -1
			unitVector.y :* -1
		End If

		Return unitVector
	End Method

	Method GetNormal:Float2()
		Local normVector:Float2 = Float2.Create(-GetYDelta(), GetXDelta())
		normVector.Normalize()

		If Not normal Then
			normVector.x :* -1
			normVector.y :* -1
		End If

		Return normVector
	End Method

	Method GetForwardNormal:Float2(other:Float2)
		Local normVector:Float2 = Float2.Create(-GetYDelta(),GetXDelta())
		Local linePointDelta:Float = (other.x - p1.x)*(p2.y - p1.y) - (other.y - p1.y)*(p2.x - p1.x)
		normVector.Normalize()

		If linePointDelta > 1 Then
			normVector.x :* -1
			normVector.y :* -1
		Else If linePointDelta = 0 Then
			normVector.x = 0
			normVector.y = 0
		End If

		Return normVector
	End Method

	Method GetNormalAngle:Float()
		Return ATan2(GetXDelta(), -GetYDelta())
	End Method

	Method IsOnNormal:Int(other:Float2)
		Return Sgn((other.x - p1.x)*(p2.y - p1.y) - (other.y - p1.y)*(p2.x - p1.x))
	End Method

	Method Scale(ratio:Double, origin:Double = 0.5)
		't1 = s - R*s
		't2 = s + R*(1 - s)
		Local t1:Double, t2:Double
		Local x1:Float, y1:Float, x2:Float, y2:Float

		'NOTE: this function causes the INTEGER points to drift due to rounding error

		t1 = origin - ratio*origin
		t2 = origin + ratio*(1 - origin)

		x1 = (1 - t1)*p1.x + t1*p2.x
		y1 = (1 - t1)*p1.y + t1*p2.y
		x2 = (1 - t2)*p1.x + t2*p2.x
		y2 = (1 - t2)*p1.y + t2*p2.y

		p1.Set(x1, y1)
		p2.Set(x2, y2)
	End Method
End Type

Type TRay
	Field focus:Float2, angle:Float

	Function Create:TRay(x:Float, y:Float, Direction:Float)
		Local newRay:TRay = New TRay
		newRay.focus = Float2.Create(x, y)
		newRay.angle = Direction

		Return newRay
	End Function

	'Moves starting point to where the mouse is
	Method MoveStartToMouse()
		focus.x = MouseX()
		focus.y = MouseY()
	End Method

	Method Draw(cvLeft, cvTop, cvRight, cvBottom)
		Local leftDist = cvLeft - focus.x
		Local topDist = cvTop - focus.y
		Local rightDist = cvRight - focus.x
		Local bottomDist = cvBottom - focus.y

		Local xDelta:Float, yDelta:Float

		xDelta = Cos(angle)
		'screen coordinates are reversed
		yDelta = -Sin(angle)

		Local edgeHorz:Float, edgeVert:Float
		Local distHorz:Float, distVert:Float

		'if positive goes right
		If xDelta >= 0 Then
			edgeHorz = cvRight
			distHorz = rightDist
		Else
			edgeHorz = cvLeft
			distHorz = leftDist
		End If

		'if positive goes down
		If yDelta >= 0 Then
			edgeVert = cvBottom
			distVert = bottomDist
		Else
			edgeVert = cvTop
			distVert = topDist
		End If

		Local x:Float, y:Float

		If Abs(distHorz / xDelta) < Abs(distVert / yDelta) Then
			x = edgeHorz
			y = Tan(angle)*(focus.x - edgeHorz) + focus.y
		Else
			y = edgeVert
			x = Tan(90 - angle)*(focus.y - edgeVert) + focus.x
		End If
		'y = tan(D)*(x - Px) + Py
		'x = tan(90 - D)*(y - Py) + Px

		DrawLine(Fix(focus.x), Fix(focus.y), Fix(x + 0.5), Fix(y))
	End Method

	Method LineTo(pt:Float2)
		DrawLine(focus.x, focus.y, pt.x, pt.y)
	End Method

	Method Move(x:Float, y:Float)

	End Method
End Type

Type TRect
	Field location:Float2
	Field size:Float2
	Field rotation:Float

	Function Create:TRect(x:Float, y:Float, width:Float, height:Float)
		Local newRect:TRect = New TRect

		newRect.location = Float2.Create(x, y)
		newRect.size = Float2.Create(width, height)

		Return newRect
	End Function

	Method Fill()
		Local oldRot:Float = GetRotation()
		SetRotation(rotation)
		DrawRect(location.x, location.y, size.x, size.y)
		SetRotation(oldRot)
	End Method

	Method Draw()
		'TODO: Change code to draw lines
		Fill()
	End Method
End Type

Type TOval
	Field location:Float2 'middle point
	Field size:Float2 'radius in x and y
	Field rotation:Float

	Function Create:TOval(x:Float, y:Float, width:Float, height:Float)
		Local newRect:TOval = New TOval

		newRect.location = Float2.Create(x, y)
		newRect.size = Float2.Create(width, height)

		Return newRect
	End Function

	Method Fill()
		Local oldRot:Float = GetRotation()
		SetRotation(rotation)
		DrawOval(location.x - size.x, location.y - size.y, size.x*2, size.y*2)
		SetRotation(oldRot)
	End Method

	Method Draw()
		'TODO: Change code to draw lines
		Fill()
	End Method

	Method IsPointInside:Int(probe:Float2)
		Local yRatio:Float = size.y/size.x
		Local dx:Float, dy:Float
		Local centerDistance:Float

		dx = location.x - probe.x
		dy = (location.y - probe.y)/yRatio
		centerDistance = Sqr(dx^2 + dy^2)

		Return centerDistance <= (size.x)
	End Method

	Method IsCircleBound:Int(probe:TOval)
		'NOTE: this method assumes both TOval are circles
		Local centerDistance:Float = location.GetDistance(probe.location)
		Local radiusSum:Float = Abs(max(size.x, size.y)) + Abs(max(probe.size.x, probe.size.y))

		Return centerDistance <= radiusSum
	End Method
End Type

'To be replaced
Type TRect2
	Field top:Float, bottom:Float, leftt:Float, rightt:Float

	Function Create:TRect2(left_loc:Float, top_loc:Float, right_loc:Float, bottom_loc:Float)
		Local newRect:TRect2 = New TRect2
		newRect.top = top_loc
		newRect.bottom = bottom_loc
		newRect.leftt = left_loc
		newRect.rightt = right_loc

		Return newRect
	End Function

	Method x:Float()
		Return leftt
	End Method

	Method y:Float()
		Return top
	End Method

	Method Move(x_loc:Float, y_loc:Float)
		Self.leftt :+ x_loc
		Self.rightt :+ x_loc
		Self.top :+ y_loc
		Self.bottom :+ y_loc
	End Method

	Method Draw()
		DrawRect(Fix(leftt), Fix(top), Fix(rightt - leftt + 1), Fix(bottom - top + 1))
	End Method

	Method Length:Float()
		Return bottom - top + 1
	End Method

	Method Width:Float()
		Return rightt - leftt + 1
	End Method

	Method IsPointInside(probe:TPoint)
		If probe.x >= leftt And probe.x <= rightt And probe.y >= top And probe.y <= bottom Then
			Return 1
		Else
			Return 0
		End If
	End Method
End Type

'To be replaced
Type TCircle
	Field h:Float, k:Float, radius:Float

	Function Create:TCircle(horz_center:Float, vert_center:Float, rad_circle:Float)
		Local newCircle:TCircle = New TCircle
		newCircle.h = horz_center
		newCircle.k = vert_center
		newCircle.radius = rad_circle

		Return newCircle
	End Function

	Method GetMidPoint:TPoint()
		Local retval:TPoint = TPoint.Create(h, k)

		Return retval
	End Method

	Method x:Float()
		Return h - radius
	End Method

	Method y:Float()
		Return k - radius
	End Method

	Method size:Float()
		Return radius*2
	End Method

	Method Move(x_loc:Float, y_loc:Float)
		Self.h :+ x_loc
		Self.k :+ y_loc
	End Method

	Method Draw()
		DrawOval(Fix(h - radius), Fix(k-radius), Fix(radius*2), Fix(radius*2))
	End Method

	Method IsPointInside(probe:TPoint)
		Local vecLen:Float
		Local dx:Float, dy:Float

		dx = h - probe.x
		dy = k - probe.y
		vecLen = Sqr(dx^2 + dy^2)

		If vecLen <= radius Then
			Return 1
		Else
			Return 0
		End If
	End Method
End Type

'*****************************************************************
' GLOBAL VARIABLES


'*****************************************************************
' FUNCTIONS
Function FindIntersection:Float2(emitter:TLine, collider:TLine)
	Local retval:Float2 = New Float2
	Local t_matrix:Float[2]
	Local s_matrix:Float[2]
	Local ext_matrix:Float[2]

	'emitter has param T and is line PQ
	'collider has param S and is line AB

	'Qx - Px = Tx
	t_matrix[0] = emitter.p2.x - emitter.p1.x
	'Qy - Py = Ty
	t_matrix[1] = emitter.p2.y - emitter.p1.y
	'Ax - Bx = Sx
	s_matrix[0] = collider.p1.x - collider.p2.x
	'Ay - By = Sy
	s_matrix[1] = collider.p1.y - collider.p2.y
	'Ax - Px = TSx
	ext_matrix[0] = collider.p1.x - emitter.p1.x
	'Ay - Py = TSy
	ext_matrix[1] = collider.p1.y - emitter.p1.y

	'determinant = Tx*Sy - Ty*Sx
	Local det:Float = t_matrix[0]*s_matrix[1] - t_matrix[1]*s_matrix[0]
	'T determinant = TSx*Sy - TSy*Sx
	Local det_t:Float = ext_matrix[0]*s_matrix[1] - ext_matrix[1]*s_matrix[0]
	'S determinant = Tx*TSy - Ty*TSx
	Local det_s:Float = t_matrix[0]*ext_matrix[1] - t_matrix[1]*ext_matrix[0]

	If det <> 0 Then
		Local t_param:Float = det_t/det
		Local s_param:Float = det_s/det

		'if det = 0 then the system is parallel or collinear
		'if t_param is within 0 and 1 (inclusive) there is a perpendicular intersection
		'if s_param is within 0 and 1 (inclusive) there is a transversal intersection

		If t_param <= 1 And t_param >= 0 And s_param <= 1 And s_param >= 0 Then
			'(1 - t)*Px + t*Qx
			'(1 - t)*Py + t*Qy
			retval.x = (1.0 - t_param)*emitter.p1.x + t_param*emitter.p2.x
			retval.y = (1.0 - t_param)*emitter.p1.y + t_param*emitter.p2.y
			'emitter.p2.x = (1 - t_param)*emitter.p1.x + t_param*emitter.p2.x
			'emitter.p2.y = (1 - t_param)*emitter.p1.y + t_param*emitter.p2.y
			Return retval
		End If
	Else
		'lines are collinear and/or parallel
		'ONLY if det_t and det_s are 0 then the lines are collinear
		If det_t = 0 And det_s = 0 Then
			Return retval
		Else
			Return Null
		End If
	End If
End Function

Function FindIntersectionSafe:Float2(emitter:TLine, collider:TLine, regressionLen:Float)
	Local retval:Float2 = New Float2
	Local t_matrix:Float[2]
	Local s_matrix:Float[2]
	Local ext_matrix:Float[2]
	Local regParam:Float

	'emitter has param T and is line PQ
	'collider has param S and is line AB

	'Qx - Px = Tx
	t_matrix[0] = emitter.p2.x - emitter.p1.x
	'Qy - Py = Ty
	t_matrix[1] = emitter.p2.y - emitter.p1.y
	'Ax - Bx = Sx
	s_matrix[0] = collider.p1.x - collider.p2.x
	'Ay - By = Sy
	s_matrix[1] = collider.p1.y - collider.p2.y
	'Ax - Px = TSx
	ext_matrix[0] = collider.p1.x - emitter.p1.x
	'Ay - Py = TSy
	ext_matrix[1] = collider.p1.y - emitter.p1.y

	'determinant = Tx*Sy - Ty*Sx
	Local det:Float = t_matrix[0]*s_matrix[1] - t_matrix[1]*s_matrix[0]
	'T determinant = TSx*Sy - TSy*Sx
	Local det_t:Float = ext_matrix[0]*s_matrix[1] - ext_matrix[1]*s_matrix[0]
	'S determinant = Tx*TSy - Ty*TSx
	Local det_s:Float = t_matrix[0]*ext_matrix[1] - t_matrix[1]*ext_matrix[0]

	If det <> 0 Then
		Local t_param:Float = det_t/det
		Local s_param:Float = det_s/det

		'if det = 0 then the system is parallel or collinear
		'if t_param is within 0 and 1 (inclusive) there is a perpendicular intersection
		'if s_param is within 0 and 1 (inclusive) there is a transversal intersection

		If t_param <= 1 And t_param >= 0 And s_param <= 1 And s_param >= 0 Then
			regParam = Sqr(regressionLen*regressionLen/(t_matrix[0]*t_matrix[0] + t_matrix[1]*t_matrix[1]))
			'(1 - t)*Px + t*Qx
			'(1 - t)*Py + t*Qy
			t_param :- regParam
			retval.x = (1.0 - t_param)*emitter.p1.x + t_param*emitter.p2.x
			retval.y = (1.0 - t_param)*emitter.p1.y + t_param*emitter.p2.y
			'emitter.p2.x = (1 - t_param)*emitter.p1.x + t_param*emitter.p2.x
			'emitter.p2.y = (1 - t_param)*emitter.p1.y + t_param*emitter.p2.y
			Return retval
		End If
	Else
		'lines are collinear and/or parallel
		'ONLY if det_t and det_s are 0 then the lines are collinear
		If det_t = 0 And det_s = 0 Then
			Return retval
		Else
			Return Null
		End If
	End If
End Function

Function FindRayIntersection:Float2(emitter:TRay, collider:TLine)
	Local retval:Float2 = New Float2
	Local t_matrix:Float[2]
	Local s_matrix:Float[2]
	Local ext_matrix:Float[2]

	'emitter has param T and is line P of angle D
	'collider has param S and is line AB

	'cos(D) = Tx
	t_matrix[0] = Cos(emitter.angle)
	'sin(D) = Ty
	t_matrix[1] = -Sin(emitter.angle)
	'Ax - Bx = Sx
	s_matrix[0] = collider.p1.x - collider.p2.x
	'Ay - By = Sy
	s_matrix[1] = collider.p1.y - collider.p2.y
	'Ax - Px = TSx
	ext_matrix[0] = collider.p1.x - emitter.focus.x
	'Ay - Py = TSy
	ext_matrix[1] = collider.p1.y - emitter.focus.y

	'determinant = Tx*Sy - Ty*Sx
	Local det:Float = t_matrix[0]*s_matrix[1] - t_matrix[1]*s_matrix[0]
	'T determinant = TSx*Sy - TSy*Sx
	Local det_t:Float = ext_matrix[0]*s_matrix[1] - ext_matrix[1]*s_matrix[0]
	'S determinant = Tx*TSy - Ty*TSx
	Local det_s:Float = t_matrix[0]*ext_matrix[1] - t_matrix[1]*ext_matrix[0]

	If det <> 0 Then
		Local t_param:Double = Double(det_t)/det
		Local s_param:Double = Double(det_s)/det

		'if det = 0 then the system is parallel or collinear
		'if t_param is greater than or equal to 0 there is a perpendicular intersection
		'if s_param is within 0 and 1 (inclusive) there is a transversal intersection

		If t_param > 0 And s_param <= 1 And s_param >= 0 Then
			'x = Px + t*cos(D)
			'y = Py + t*sin(D)
			retval.x = emitter.focus.x + t_param*Cos(emitter.angle)
			retval.y = emitter.focus.y - t_param*Sin(emitter.angle)
			Return retval
		End If
	Else
		'lines are collinear and/or parallel
		'ONLY if det_t an det_s are 0 then the lines are collinear
	End If
End Function

Function Fix:Int(val:Float)
	Return Int(val + 0.5*Sgn(val))
End Function

'*****************************************************************
' INITIALIZATION
