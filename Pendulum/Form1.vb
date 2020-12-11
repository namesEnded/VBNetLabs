Imports Microsoft.VisualBasic.PowerPacks
Public Class Form1
    Dim armLenght, angle, mass, firstAngle, bobX, bobY, aVel, aAcc, cyclicFrequency, oscillationPeriod, dampingFactor, msSW As Double
    Dim T As Double

    Dim sSW, mSW, hSW As Integer
    Dim mmsSW, ssSW, mmSW, hhSW As String

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        oscillationPeriodLbl.Text = "0"
        cyclicFrequencyLbl.Text = "0"
        dampingFactorLbl.Text = "0"
        fullEnergyAtStartLbl.Text = "0"
        fullEnergyLbl.Text = "0"
        kineticEnergyLbl.Text = "0"
        potentialEnergyLbl.Text = "0"
        oscillationPeriodPrewLbl.Text = "0"
        curVelLbl.Text = "0"
        perTimeLbl.Text = "00:00:00:00"
    End Sub

    Private Sub Label1_Click(sender As Object, e As EventArgs) Handles Label1.Click

    End Sub

    'Const dt As Double = 0.0167
    Const dt As Double = 0.002
    'Const dt As Double = 0.03
    Const angleСorrection As Double = 90
    Const angleDegrToRad As Double = (Math.PI / 180)
    Const angleRadToDegr As Double = (180 / Math.PI)
    Const g As Double = 9.81


    Private Sub setBtn_Click(sender As Object, e As EventArgs) Handles setBtn.Click
        angle = angleValue.Text
        armLenght = armLenghtValue.Text

        If resistanceCoeffValue.Text <> "" Then
            dampingFactor = resistanceCoeffValue.Text / (2 * mass)
            dampingFactorLbl.Text = dampingFactor
            cyclicFrequency = Math.Sqrt(9.8 / (armLenghtLbl.Text / 100))
            cyclicFrequency = Math.Sqrt(cyclicFrequency ^ 2 - dampingFactor ^ 2)
            oscillationPeriod = (2 * Math.PI) / cyclicFrequency
        Else
            cyclicFrequency = Math.Sqrt(9.8 / (armLenghtLbl.Text / 100))
            oscillationPeriod = (2 * Math.PI) / cyclicFrequency
        End If
        cyclicFrequencyLbl.Text = cyclicFrequency
        oscillationPeriodLbl.Text = oscillationPeriod


        bobX = (arm.StartPoint.X + armLenght * Math.Sin(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        bobY = (arm.StartPoint.Y + armLenght * Math.Cos(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        bob.Location = New Point(bobX, bobY)
        arm.EndPoint = New Point(bobX + (bob.Size.Width / 2), bobY + (bob.Size.Width / 2))
    End Sub

    Private Sub periodTmr_Tick(sender As Object, e As EventArgs) Handles periodTmr.Tick
        msSW += 1
        If msSW < 10 Then
            mmsSW = "00" & msSW.ToString
        ElseIf msSW < 100 Then
            mmsSW = "0" & msSW.ToString
            mmsSW = msSW.ToString
        Else
            mmsSW = msSW.ToString
        End If
        perTimeLbl.Text = hhSW & ":" & mmSW & ":" & ssSW & ":" & mmsSW
        If msSW = 99 Then
            msSW = 0
            sSW += 1
            If sSW < 10 Then
                ssSW = "0" & sSW.ToString
            Else
                ssSW = sSW.ToString
            End If
            perTimeLbl.Text = hhSW & ":" & mmSW & ":" & ssSW & ":" & mmsSW
            If sSW = 60 Then
                sSW = 0
                mSW += 1
                If mSW < 10 Then
                    mmSW = "0" & mSW.ToString
                Else
                    mmSW = mSW.ToString
                End If
                perTimeLbl.Text = hhSW & ":" & mmSW & ":" & ssSW & ":" & mmsSW
                If mSW = 60 Then
                    mSW = 0
                    hSW += 1
                    If hSW < 10 Then
                        hhSW = "0" & hSW.ToString
                    Else
                        hhSW = hSW.ToString
                    End If
                    perTimeLbl.Text = hhSW & ":" & mmSW & ":" & ssSW & ":" & mmsSW
                End If
            End If
        End If
    End Sub

    Private Sub bob_MouseMove(sender As Object, e As MouseEventArgs) Handles bob.MouseMove
        Dim curLcn As Point
        If Control.MouseButtons = Windows.Forms.MouseButtons.Left Then
            CType(sender, OvalShape).Left = Control.MousePosition.X - Me.Left - (CType(sender, OvalShape).Width - 21.5)
            CType(sender, OvalShape).Top = Control.MousePosition.Y - Me.Top - (CType(sender, OvalShape).Height + 1.5)
            curLcn = bob.Location
            arm.EndPoint = New Point(curLcn.X + (bob.Size.Width / 2), curLcn.Y + (bob.Size.Width / 2))
        End If
    End Sub

    Private Sub getCurDataPlaceBtn_Click(sender As Object, e As EventArgs) Handles getCurDataPlaceBtn.Click
        armLenghtLbl.Text = Math.Sqrt((arm.EndPoint.X - arm.StartPoint.X) ^ 2 + (arm.EndPoint.Y - arm.StartPoint.Y) ^ 2)
        angleLbl.Text = (Math.Asin((bob.Location.X + (bob.Size.Width / 2) - arm.StartPoint.X) / armLenghtLbl.Text)) * angleRadToDegr
    End Sub

    Private Sub setGetCurDataPlaceBtn_Click(sender As Object, e As EventArgs) Handles setGetCurDataPlaceBtn.Click
        armLenghtLbl.Text = Math.Sqrt((arm.EndPoint.X - arm.StartPoint.X) ^ 2 + (arm.EndPoint.Y - arm.StartPoint.Y) ^ 2)
        angleLbl.Text = (Math.Asin((bob.Location.X + (bob.Size.Width / 2) - arm.StartPoint.X) / armLenghtLbl.Text)) * angleRadToDegr

        angle = angleLbl.Text
        armLenght = armLenghtLbl.Text
        angleValue.Text = angleLbl.Text
        armLenghtValue.Text = armLenghtLbl.Text

        If resistanceCoeffValue.Text <> "" Then
            dampingFactor = resistanceCoeffValue.Text / (2 * mass)
            dampingFactorLbl.Text = dampingFactor
            cyclicFrequency = Math.Sqrt(9.8 / (armLenghtLbl.Text / 100))
            cyclicFrequency = Math.Sqrt(cyclicFrequency ^ 2 - dampingFactor ^ 2)
            oscillationPeriod = (2 * Math.PI) / cyclicFrequency
        Else
            cyclicFrequency = Math.Sqrt(9.8 / (armLenghtLbl.Text / 100))
            oscillationPeriod = (2 * Math.PI) / cyclicFrequency
        End If

        cyclicFrequencyLbl.Text = cyclicFrequency
        oscillationPeriodLbl.Text = oscillationPeriod

        bobX = (arm.StartPoint.X + armLenght * Math.Sin(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        bobY = (arm.StartPoint.Y + armLenght * Math.Cos(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        bob.Location = New Point(bobX, bobY)
        arm.EndPoint = New Point(bobX + (bob.Size.Width / 2), bobY + (bob.Size.Width / 2))

    End Sub

    Private Sub startBtn_Click(sender As Object, e As EventArgs) Handles startBtn.Click
        If startBtn.Text = "Start" Then
            moveTmr.Start()
            periodTmr.Start()
            aVel = 0
            aAcc = 0
            mass = 1
            hSW = 0
            mSW = 0
            sSW = 0
            msSW = 0
            hhSW = "00"
            mmSW = "00"
            ssSW = "00"
            mmsSW = "00"

            fullEnergyAtStartLbl.Text = (mass * (g / (armLenght / 100)) * (angle * angleRadToDegr * (armLenght / 100)) ^ 2) / 2 * (armLenght / 100)
            firstAngle = (Math.Asin((bob.Location.X + (bob.Size.Width / 2) - arm.StartPoint.X) / (armLenghtLbl.Text))) * angleRadToDegr
            startBtn.Text = "Stop"
        Else
            moveTmr.Stop()
            periodTmr.Stop()
            hSW = 0
            mSW = 0
            sSW = 0
            msSW = 0
            perTimeLbl.Text = hhSW & ":" & mmSW & ":" & ssSW & ":" & mmsSW
            startBtn.Text = "Start"
        End If
    End Sub
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        moveTmr.Enabled = False
        Button1.Cursor = Cursors.SizeAll
        bob.Cursor = Cursors.SizeAll
        T = 0
    End Sub

    Private Sub moveTmr_Tick(sender As Object, e As EventArgs) Handles moveTmr.Tick
        Dim kineticEnergy, potentialEnergy, fullEnergy As Double

        ''aVel = Math.Sqrt(g / armLenght) * (angle * armLenght)
        'aAcc = -((g / armLenght) * (angle * armLenght))
        'aVel += aAcc
        'angle += aVel / (Math.Sqrt(g / armLenght) * armLenght)
        ''aAcc = -(g / armLenght) * Math.Sin(angle * angleDegrToRad)
        ''aVel += aAcc * dt
        ''angle += aVel * dt
        ''T += dt
        ''perTimeLbl.Text = T

        aAcc = -((g / armLenght) * (angle * armLenght))
        aVel += aAcc * dt
        If resistanceCoeffValue.Text <> "" Then
            aVel *= 1 - resistanceCoeffValue.Text
        End If
        curVelLbl.Text = aVel
        angle += (aVel / (Math.Sqrt(g / armLenght) * armLenght))

        kineticEnergy = (mass * aVel ^ 2) / 2
        kineticEnergyLbl.Text = kineticEnergy
        potentialEnergy = (mass * (g / (armLenght / 100)) * (angle * angleRadToDegr * (armLenght / 100)) ^ 2) / 2 * (armLenght / 100)
        potentialEnergyLbl.Text = potentialEnergy
        fullEnergy = kineticEnergy + potentialEnergy
        fullEnergyLbl.Text = fullEnergy

        bobX = (arm.StartPoint.X + armLenght * Math.Sin(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        bobY = (arm.StartPoint.Y + armLenght * Math.Cos(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        bob.Location = New Point(bobX, bobY)
        arm.EndPoint = New Point(bobX + (bob.Size.Width / 2), bobY + (bob.Size.Width / 2))

        'aAcc = -((g / armLenght) * Math.Sin(angle * angleDegrToRad)
        'angle +=aVel*dt 
        'aVel += aAcc * dt

        If Math.Round(firstAngle, 2) = Math.Round(angle, 2) And aVel <> 0 Then
            'moveTmr.Stop()
            oscillationPeriodPrewLbl.Text = perTimeLbl.Text
            'periodTmr.Stop()
        End If
    End Sub

    Private Sub Button1_MouseMove(sender As Object, e As MouseEventArgs) Handles Button1.MouseMove
        If Control.MouseButtons = Windows.Forms.MouseButtons.Left Then
            CType(sender, Button).Left = Control.MousePosition.X - Me.Left - CType(sender, Button).Width
            CType(sender, Button).Top = Control.MousePosition.Y - Me.Top - CType(sender, Button).Height
        End If
    End Sub
End Class
