' Module:
' Purpose:
' Programmer:	Mario D. Flores
' Created:		ddd, mmm dd, yyyy

'*****************************************************************
' DIRECTIVES
Strict

'*****************************************************************
' NOTES


'*****************************************************************
' EXTERNAL LIBRAIRES


'*****************************************************************
' GLOBAL CONSTANTS
'pre-declared for for-loops
Local i:Int

'*****************************************************************
' DYNAMIC TYPES

Rem
	Float2 is a facility for operations with 2-dimensional parallel data. It may
	sometimes behave like a vector, but most operations are componentwise. I might
	later add dot products and cross products.
End Rem
Type Float2
	Field x:Float, y:Float

	Function Create:Float2(x_val:Float, y_val:Float)
		Local newval:Float2 = New Float2
		newval.x = x_val
		newval.y = y_val

		Return newval
	End Function

	'Mutator Methods

	'Set a vector by its indices
	Method Set(x_val:Float, y_val:Float)
		x = x_val
		y = y_val
	End Method

	'Set vector indices with deadzones (zero based vector)
	Method SetWLimit(x_val:Float, y_val:Float, dzLow:Float, dzHigh:Float)
		Local x2:Float = Abs(x_val)
		Local y2:Float = Abs(y_val)

		If x2 < dzLow Then x_val = 0
		If y2 < dzLow Then y_val = 0
		If x2 > dzHigh Then x_val = Sgn(x_val) * dzHigh
		If y2 > dzHigh Then y_val = Sgn(y_val) * dzHigh

		x = x_val
		y = y_val
	End Method
	
	'Set vector indices with deadzones using a polar limit
	Method SetWPLimit(x_delta:Float, y_delta:Float, rdLow:Float, rdHigh:Float)
		Local x2:Float = x_delta
		Local y2:Float = y_delta
		Local sqSum:Float = Sqr(x2*x2 + y2*y2)
		Local t_param:Float

		If sqSum < rdLow Then
			x = 0
			y = 0
		Else If sqSum > rdHigh Then
			t_param = Sqr(rdHigh*rdHigh)/sqSum
			x = x2*t_param
			y = y2*t_param
		Else
			x = x2
			y = y2
		End If
	End Method

	'Increase rectangular indices of vector
	Method Increase(x_delta:Float, y_delta:Float)
		x :+ x_delta
		y :+ y_delta
	End Method

	'Increase rectangular indices of vector, and returns previous location
	Method IncreaseRet(x_delta:Float, y_delta:Float, prevLoc:Float2)
		prevLoc.Set(x, y)
		x :+ x_delta
		y :+ y_delta
	End Method

	'Increase rectangular indices with polar limit (radius)
	Method IncreaseWPLimit(x_delta:Float, y_delta:Float, rad_lim:Float)
		Local x2:Float = x + x_delta
		Local y2:Float = y + y_delta
		Local sqSum:Float = x2*x2 + y2*y2
		Local t_param:Float

		If Sqr(sqSum) > rad_lim Then
			t_param = Sqr(rad_lim*rad_lim/sqSum)
			x = t_param*x2
			y = t_param*y2
		Else
			x = x2
			y = y2
		End If
	End Method

	'Set vector with polar coordinates
	Method SetPolar(radius:Float, theta:Float)
			x = radius * Cos(theta)
			y = radius * Sin(theta)
	End Method

	'Set magnitude of vector without changing angle
	Method SetMagnitude(newMag:Float)
		Local angle:Float = ATan2(y, x)

		x = newMag * Cos(angle)
		y = newMag * Sin(angle)
	End Method

	'Set angle of vector without changing magnitude
	Method SetAngle:Float(newAngle:Float)
		Local magnitude:Float = Sqr(x*x + y*y)

		x = magnitude * Cos(newAngle)
		y = magnitude * Sin(newAngle)
	End Method

	'Accessor Methods

	'Returns the magnitude (or absolute value) of the vector
	Method GetMagnitude:Float()
		Return Sqr(x*x + y*y)
	End Method

	'Returns the angle (or argument) of the vector
	Method GetAngle:Float()
		Return ATan2(y, x)
	End Method

	'Finds the delta between this vector and another one (in magnitude).
	Method GetDistance:Float(other:Float2)
		Local xDelta:Float = x - other.x
		Local yDelta:Float = y - other.y

		Return Sqr(xDelta*xDelta + yDelta*yDelta)
	End Method

	'Arithmetic Methods
	Rem
		NOTE: consider moving chained operations outside of the definition of Float2
		This is because no data inside the class is being modified, and, therefore,
		having such opeartions here violates encapsulation. Replace them with self
		operations of the same nature.
	EndRem

	'Chained componentwise addition of vectors
	Method Add:Float2(rhs:Float2)
		Local retval:Float2 = Float2.Create(x + rhs.x, y + rhs.y)
		Return retval
	End Method

	'Chained componentwise subtraction of vectors
	Method Subtract:Float2(rhs:Float2)
		Local retval:Float2 = Float2.Create(x - rhs.x, y - rhs.y)
		Return retval
	End Method

	'Chained componentwise multiplication of vectors
	Method Multiply:Float2(rhs:Float2)
		Local retval:Float2 = Float2.Create(x * rhs.x, y * rhs.y)
		Return retval
	End Method

	'Chained componentwise division of vectors
	Method Divide:Float2(rhs:Float2)
		Local retval:Float2 = Float2.Create(x / rhs.x, y / rhs.y)
		Return retval
	End Method

	'Chained componentwise square root of vector
	Method Sqrt:Float2()
		Local retval:Float2 = Float2.Create(Sqr(x), Sqr(y))
		Return retval
	End Method

	'Chained componentwise square of vector
	Method Pow2:Float2()
		Local retval:Float2 = Float2.Create(x * x, y * y)
		Return retval
	End Method

	'Chained componentwise cube of vector
	Method Pow3:Float2()
		Local retval:Float2 = Float2.Create(x * x * x, y * y * y)
		Return retval
	End Method

	'Self scalar-vector multiplication
	Method ScalarMult:Float(scalar:Float)
		Local retval:Float2 = Float2.Create(x * scalar, y * scalar)
	End Method

	'Returns the dot product of self • rhs
	Method DotProduct:Float(rhs:Float2)
		Return x * rhs.x + y * rhs.y
	End Method

	'Transformations

	'Sets vector to its equivalent unit vector
	Method Normalize()
		Local magnitude:Float = GetMagnitude()
		If magnitude > 0 Then
			x :/ magnitude
			y :/ magnitude
		End If
	End Method

	'Transforms the vector with a given 2x2 matrix
	Method Transform(mtx:Matrix2)
		'x = x*a + y*b
		'y = x*c + y*d
		x = x*mtx.mtx[0, 0] + y*mtx.mtx[0, 1]
		y = x*mtx.mtx[1, 0] + y*mtx.mtx[1, 1]
	End Method

	'Homogeneous transformation
	'a b c     x
	'd e f  *  y
	'0 0 1     1
	'Transforms the vector with a given 3x3 matrix. It allows translation.
	Method TransformH(mtx:Matrix3)
		'x = x*a + y*b + 1*c
		'y = x*d + y*e + 1*f
		'1 = 0 + 0 + 1
		x = x*mtx.mtx[0, 0] + y*mtx.mtx[0, 1] + mtx.mtx[0, 2]
		y = x*mtx.mtx[1, 0] + y*mtx.mtx[1, 1] + mtx.mtx[1, 2]
	End Method
End Type

Type Float3
	Field x:Float, y:Float, z:Float

	Function Create:Float3(x_val:Float, y_val:Float, z_val:Float)
		Local newval:Float3 = New Float3
		newval.x = x_val
		newval.y = y_val
		newval.z = z_val

		Return newval
	End Function

	Method Set(x_val:Float, y_val:Float, z_val:Float)
		x = x_val
		y = y_val
		z = z_val
	End Method

	Method SetSpherical(radius:Float, theta:Float, phi:Float)
			'https://en.wikipedia.org/wiki/Spherical_coordinate_system#Cartesian_coordinates'
			x = radius * Sin(theta) * Cos(phi)
			y = radius * Sin(theta) * Sin(phi)
			z = radius * Cos(theta)
	End Method

	Method Increase(x_loc:Float, y_loc:Float, z_loc:Float)
		x :+ x_loc
		y :+ y_loc
		z :+ z_loc
	End Method

	Method GetMagnitude:Float()
		Return Sqr(x*x + y*y + z*z)
	End Method

	Method GetAngle:Float2()
		Local retval:Float2 = New Float2
		retval.x = ACos(z/GetMagnitude())
		retval.y = ATan2(y, x)
		Return retval
	End Method

	Method Add:Float3(rhs:Float3)
		Local retval:Float3 = Float3.Create(x + rhs.x, y + rhs.y, z + rhs.z)
		Return retval
	End Method

	Method Subtract:Float3(rhs:Float3)
		Local retval:Float3 = Float3.Create(x - rhs.x, y - rhs.y, z - rhs.z)
		Return retval
	End Method

	Method Multiply:Float3(rhs:Float3)
		Local retval:Float3 = Float3.Create(x * rhs.x, y * rhs.y, z * rhs.z)
		Return retval
	End Method

	Method Divide:Float3(rhs:Float3)
		Local retval:Float3 = Float3.Create(x / rhs.x, y / rhs.y, z / rhs.z)
		Return retval
	End Method

	Method Sqrt:Float3()
		Local retval:Float3 = Float3.Create(Sqr(x), Sqr(y), Sqr(z))
		Return retval
	End Method

	Method Pow2:Float3()
		Local retval:Float3 = Float3.Create(x * x, y * y, z * z)
		Return retval
	End Method

	Method Pow3:Float3()
		Local retval:Float3 = Float3.Create(x * x * x, y * y * y, z * z * z)
		Return retval
	End Method

	Method Transform(mtx:Matrix3)
		'Apply transformation to this float here
	End Method

	'Homogeneous transformation
	'a b c d     x
	'e f g h  *  y
	'i j k l     z
	'0 0 0 1     1
	Method TransformH(mtx:Matrix4)
		'Apply transformation to this vector here
	End Method
End Type

Type Matrix2
	Const MTX_SIZE:Int = 2
	Field mtx:Float[2, 2]
	'row, column
	'[0, 0]	[0, 1]
	'[1, 0] [1, 1]

	Function Create:Matrix2(values:Float[,])
		Local retval:Matrix2 = New Matrix2

		retval.mtx[0, 0] = values[0, 0]
		retval.mtx[0, 1] = values[0, 1]
		retval.mtx[1, 0] = values[1, 0]
		retval.mtx[1, 1] = values[1, 1]

		Return retval
	End Function

	Method Add(rhs:Matrix2)
		Local ix:Int, iy:Int

		For iy = 0 To 1
			For ix = 0 To 1
				mtx[ix, iy] :+ rhs.mtx[ix, iy]
			Next
		Next
	End Method
End Type

Type Matrix3
	Const MTX_SIZE:Int = 3
	Field mtx:Float[3, 3]
	'row, column
	'[0, 0] [0, 1] [0, 2]
	'[1, 0] [1, 1] [1, 2]
	'[2, 0] [2, 1] [2, 2]

	Function Create:Matrix3(values:Float[,])
		Local retval:Matrix3 = New Matrix3

		retval.mtx[0, 0] = values[0, 0]
		retval.mtx[0, 1] = values[0, 1]
		retval.mtx[0, 2] = values[0, 2]
		retval.mtx[1, 0] = values[1, 0]
		retval.mtx[1, 1] = values[1, 1]
		retval.mtx[1, 2] = values[1, 2]
		retval.mtx[2, 0] = values[2, 0]
		retval.mtx[2, 1] = values[2, 1]
		retval.mtx[2, 2] = values[2, 2]

		Return retval
	End Function

	Method Add(rhs:Matrix3)
		Local ix:Int, iy:Int

		For iy = 0 To 2
			For ix = 0 To 2
				mtx[ix, iy] :+ rhs.mtx[ix, iy]
			Next
		Next
	End Method
End Type

Type Matrix4
	Const MTX_SIZE:Int = 4
	Field mtx:Float[4, 4]
	'row, column
	'[0, 0] [0, 1] [0, 2] [0, 3]
	'[1, 0] [1, 1] [1, 2] [1, 3]
	'[2, 0] [2, 1] [2, 2] [2, 3]
	'[3, 0] [3, 1] [3, 2] [3, 3]

	Function Create:Matrix4(values:Float[,])
		Local retval:Matrix4 = New Matrix4

		retval.mtx[0, 0] = values[0, 0]
		retval.mtx[0, 1] = values[0, 1]
		retval.mtx[0, 2] = values[0, 2]
		retval.mtx[0, 3] = values[0, 3]
		retval.mtx[1, 0] = values[1, 0]
		retval.mtx[1, 1] = values[1, 1]
		retval.mtx[1, 2] = values[1, 2]
		retval.mtx[1, 3] = values[1, 3]
		retval.mtx[2, 0] = values[2, 0]
		retval.mtx[2, 1] = values[2, 1]
		retval.mtx[2, 2] = values[2, 2]
		retval.mtx[2, 3] = values[2, 3]
		retval.mtx[3, 0] = values[3, 0]
		retval.mtx[3, 1] = values[3, 1]
		retval.mtx[3, 2] = values[3, 2]
		retval.mtx[3, 3] = values[3, 3]

		Return retval
	End Function

	Method Add(rhs:Matrix4)
		Local ix:Int, iy:Int

		For iy = 0 To 3
			For ix = 0 To 3
				mtx[ix, iy] :+ rhs.mtx[ix, iy]
			Next
		Next
	End Method
End Type

'*****************************************************************
' STATIC TYPES
Type TrigMathF
	Rem
		Triangular wave fully defined by its properties.
	EndRem
	Function TriWaveFull:Float(x:Float, amplitude:Float, period:Float)
		Return 2*period/amplitude*Abs(Modulo(x, period) - period*0.5) - 2*amplitude*0.25
	End Function

	Rem
		Triangular wave with period 1 and amplitude 1
	EndRem
	Function TriWave:Float(x:Float)
		Return Abs(Modulo(2*x, 2) - 1) - 0.5
	End Function

	Rem
		The Mod operation in BlitzMax returns the remainder. This is a number that
		satisfies:
			a = q*b + r, q = truncate(a/b)
		when solved for r:
			r = a - truncate(q)*b
		This means that the result is always the same sign as a. From a mathematical
		perspective this is not wrong but it is not stable. The formal definition in
		modulo arithmetic is:
			a = q*b + r, q = floor(a/b)
		when solved for r:
			r = a - floor(q)*b
		That is what this function does, and the sign of the output is always the
		same as b.
	EndRem
	Function Modulo:Float(div:Float, modl:Float)
		If div > 0 Then
			Return div Mod modl
		End If

		Return div Mod modl + modl
	End Function

	Rem
		There is already a Max function in BlitzMax, that one is faster. This one is
		provided just for completness sake.
	End Rem
	Function GetMax:Float(n1:Float, n2:Float)
		If n1 >= n2 Then
			Return n1
		Else
			Return n2
		End If
	End Function

	Rem
		There is already a Min function in BlitzMax, that one is faster. This one is
		provided just for completness sake.
	End Rem
	Function GetMin:Float(n1:Float, n2:Float)
		If n1 <= n2 Then
			Return n1
		Else
			Return n2
		End If
	End Function
End Type

Type MathAlg
	Function Lerp:Float(x0:Float, x1:Float, t:Float)
		Return (1 - t)*x0 + t*x1
	End Function

	Function Qerp:Float(x0:Float, x1:Float, x2:Float, t:Float)
		Local t_inv:Float = 1 - t
		Return t_inv*t_inv*x0 + t_inv*t*x1 + t*t*x2
	End Function
End Type

'*****************************************************************
' GLOBAL VARIABLES


'*****************************************************************
' FUNCTIONS
'Returns a new vector which is the projection of self onto other
Function GetProjection:Float2(vect:Float2, reciever:Float2)
	Local recHat:Float2 = Float2.Create(reciever.x, reciever.y)
	recHat.Normalize()

	Local vecMagnitude:Float = vect.GetMagnitude()
	Local trigVal:Float = Cos(vect.GetAngle() - recHat.GetAngle())
	Local retval:Float2 = Float2.Create(0, 0)

	retval.x = vecMagnitude*trigVal*recHat.x
	retval.y = vecMagnitude*trigVal*recHat.y

	Return retval
End Function

'*****************************************************************
' INITIALIZATION
