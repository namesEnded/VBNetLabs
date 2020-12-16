Imports Microsoft.VisualBasic.PowerPacks
Public Class Form1
    Dim armLenght, angle, mass, firstAngle, bobX, bobY, aVel, aAcc, fEnSt, prewAngle, cyclicFrequency, oscillationPeriod, numberTotalVibrations, dampingFactor, fullEnergyAtStart, fullEnergy, msSW, xMax, angleAtStart As Double
    Dim omega, theta, dt, x, y, T, L, alpha, sysH As Double

    Private Sub Label39_Click(sender As Object, e As EventArgs) Handles Label39.Click

    End Sub

    Private Sub Label17_Click(sender As Object, e As EventArgs) Handles Label17.Click

    End Sub

    Dim perFlag, perFlag2, perFlag3 As Boolean
    Const deltaT As Double = 0.1
    Dim FlagB0, FlagM0, FlagB As Boolean

    'Const dt As Double = 0.0167
    'Const dt As Double = 0.02
    'Const dt As Double = 0.005
    'Const dt As Double = 1 / 1000
    'Const dt As Double = 0.01
    Const angleСorrection As Double = 90

    Private Sub newMethodBtn_Click(sender As Object, e As EventArgs) Handles newMethodBtn.Click
        Dim resistanceCoeff As Double
        If newMethodBtn.Text = "newMethod" Then
            mass = massValue.Text
            L = armLenghtValue.Text
            angleAtStart = angleValue.Text * angleDegrToRad
            theta = angleAtStart
            omega = 0
            T = 0
            FlagB = False
            FlagM0 = False
            FlagB0 = False
            numberOfVibrationsLbl.Text = 0
            sysH = (L / 100) + arm.StartPoint.Y / 100 - (bob.Size.Width / 2) / 100
            If resistanceCoeffValue.Text <> "0" Then
                dampingFactor = resistanceCoeffValue.Text / (2 * mass)
                dampingFactorLbl.Text = dampingFactor
                cyclicFrequency = Math.Sqrt(((Math.Sqrt(9.81 / (L / 100))) ^ 2) - ((dampingFactor) ^ 2))
                oscillationPeriod = (2 * Math.PI) / cyclicFrequency
                tPer.Text = oscillationPeriod
                tCicl.Text = cyclicFrequency
            Else
                    oscillationPeriod = 2 * Math.PI * Math.Sqrt((L / 100) / 9.81)
                tPer.Text = oscillationPeriod
                cyclicFrequency = Math.Sqrt(981 / L)
                tCicl.Text = cyclicFrequency
            End If
            'fullEnergyAtStart = mass * 9.81 * L / 100 * (1 - Math.Cos(theta))
            fullEnergyAtStart = mass * 9.81 * (sysH - (bob.Location.Y / 100))
            fEnStart.Text = fullEnergyAtStart
            TmoveTmrTwo.Start()
            newMethodBtn.Text = "Stop"
        Else
            newMethodBtn.Text = "newMethod"
            TmoveTmrTwo.Stop()
        End If
    End Sub

    Const angleDegrToRad As Double = (Math.PI / 180)
    Const angleRadToDegr As Double = (180 / Math.PI)
    Const g As Double = 9.81

    Private Sub ComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox1.SelectedIndexChanged
        If ComboBox1.Text = "Нет" Then
            resistanceCoeffValue.Enabled = True
            resistanceCoeffValue.BackColor = Color.Gray
        End If
        If ComboBox1.Text = "Воздух" Then
            resistanceCoeffValue.Text = 1.2
            resistanceCoeffValue.Enabled = False

        End If
        If ComboBox1.Text = "Вода" Then
            resistanceCoeffValue.Text = 1000
            resistanceCoeffValue.Enabled = False
            'PictureBox1.BackColor = Color.Blue
        End If
        If ComboBox1.Text = "Спирт" Then
            resistanceCoeffValue.Text = 800
            resistanceCoeffValue.Enabled = False
            'PictureBox1.BackColor = Color.Blue
        End If
        If ComboBox1.Text = "Бензин" Then
            resistanceCoeffValue.Text = 710
            resistanceCoeffValue.Enabled = False
            'PictureBox1.BackColor = Color.Blue
        End If
        If ComboBox1.Text = "Серная кислота" Then
            resistanceCoeffValue.Text = 1800
            resistanceCoeffValue.Enabled = False
            'PictureBox1.BackColor = Color.Blue
        End If
        If ComboBox1.Text = "Радон" Then
            resistanceCoeffValue.Text = 9.73
            resistanceCoeffValue.Enabled = False
            'PictureBox1.BackColor = Color.Blue
        End If


    End Sub

    Private Sub TmoveTmrTwo_Tick(sender As Object, e As EventArgs) Handles TmoveTmrTwo.Tick
        Const dt As Double = 0.01
        Dim theta_mid, omega_mid, alpha_mid, fEnergy, kineticEnergy, potentialEnergy, V, prewTheta As Double
        alpha = -981 / L * Math.Sin(theta)
        theta_mid = theta + omega * 0.5 * dt
        omega_mid = omega + alpha * 0.5 * dt
        alpha_mid = -981 / L * Math.Sin(theta_mid)
        T += dt
        prewTheta = theta
        If resistanceCoeffValue.Text <> "0" Then
            omega += alpha_mid * dt * (1 - dampingFactor)
            theta += omega_mid * dt
            curAngleLbl.Text = theta
            'potentialEnergy = mass * 9.81 * (L / 100) * (1 - Math.Cos(theta))
            'kineticEnergy = (mass * (L / 100) ^ 2 * (theta * angleRadToDegr) ^ 2) / 2
            potentialEnergy = mass * 9.81 * (sysH - (bob.Location.Y / 100))
            kineticEnergy = (((1 / 2) * mass * (((theta * angleRadToDegr - prewTheta * angleRadToDegr)) / deltaT) ^ 2 * (L / 100) ^ 2)) * 0.0304132
            fEnergy = potentialEnergy + kineticEnergy
            fEn.Text = fEnergy
            kinEn.Text = kineticEnergy
            pEn.Text = potentialEnergy
            If theta > 0 Then
                FlagB0 = True
            End If
            If theta < 0 And FlagB0 = True Then
                FlagM0 = True
            End If
            If theta > 0 And FlagB0 = True And FlagM0 = True Then
                FlagB = True
            End If
            If FlagB = True And FlagM0 = True And FlagB0 = True Then
                numberOfVibrationsLbl.Text += 1
                FlagB = False
                FlagM0 = False
                FlagB0 = False
            End If
        Else
            omega += alpha_mid * dt
            theta += omega_mid * dt
            curAngleLbl.Text = theta * angleRadToDegr
            'potentialEnergy = mass * 9.81 * (L / 100) * (1 - Math.Cos(theta))
            sshLbl.Text = sysH
            yLbl.Text = bob.Location.Y / 100
            oscillationPeriodPrewLbl.Text = prewTheta * angleRadToDegr
            Label10.Text = theta * angleRadToDegr
            potentialEnergy = mass * 9.81 * (sysH - (bob.Location.Y / 100))
            'kineticEnergy = (((1 / 2) * mass * (((theta * angleRadToDegr - prewTheta * angleRadToDegr)) / deltaT) ^ 2 * (L / 100) ^ 2) / 32.69773) / 1.00559
            kineticEnergy = (((1 / 2) * mass * (((theta * angleRadToDegr - prewTheta * angleRadToDegr)) / deltaT) ^ 2 * (L / 100) ^ 2)) * 0.0304132
            Label22.Text = fullEnergyAtStart - potentialEnergy
            'kineticEnergy = fullEnergyAtStart - potentialEnergy
            Label28.Text = Math.Sqrt((2 * Label22.Text) / (mass))
            kinEn.Text = kineticEnergy
            pEn.Text = potentialEnergy
            fEnergy = potentialEnergy + kineticEnergy
            fEn.Text = fEnergy
            If theta > 0 Then
                FlagB0 = True
            End If
            If theta < 0 And FlagB0 = True Then
                FlagM0 = True
            End If
            If theta > 0 And FlagB0 = True And FlagM0 = True Then
                FlagB = True
            End If
            If FlagB = True And FlagM0 = True And FlagB0 = True Then
                numberOfVibrationsLbl.Text += 1
                FlagB = False
                FlagM0 = False
                FlagB0 = False
            End If
        End If
        fVel.Text = omega
        x = L * Math.Sin(theta)
        y = L * Math.Cos(theta)
        ' potentialEnergy = (mass ^ 2 * 9.81 * L / 100 ^ 2 * Math.Cos(theta) * omega) / 2
        V = Math.Sqrt((2 * kineticEnergy) / (mass))
        enVel.Text = V

        bobX = (arm.StartPoint.X + L * Math.Sin(theta)) - (bob.Size.Width / 2)
        bobY = (arm.StartPoint.Y + L * Math.Cos(theta)) - (bob.Size.Width / 2)
        bob.Location = New Point(bobX, bobY)
        arm.EndPoint = New Point(bobX + (bob.Size.Width / 2), bobY + (bob.Size.Width / 2))

        'bobX = (arm.StartPoint.X + x) - (30)
        'bobY = (arm.StartPoint.Y + y) - (30)
        'bob.Location = New Point(bobX, bobY)
        'arm.EndPoint = New Point(bobX + (bob.Size.Width / 2), bobY + (bob.Size.Width / 2))

    End Sub

    Dim sSW, mSW, hSW As Integer
    Dim mmsSW, ssSW, mmSW, hhSW As String



    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        oscillationPeriodLbl.Text = "0"
        cyclicFrequencyLbl.Text = "0"
        dampingFactorLbl.Text = "0"
        fullEnergyAtStartLbl.Text = "0"
        curFullEnergyLbl.Text = "0"
        kineticEnergyLbl.Text = "0"
        potentialEnergyLbl.Text = "0"
        oscillationPeriodPrewLbl.Text = "0"
        curVelLbl.Text = "0"
        perTimeLbl.Text = "00:00:00:00"
    End Sub





    Private Sub setBtn_Click(sender As Object, e As EventArgs) Handles setBtn.Click
        angle = angleValue.Text
        armLenght = armLenghtValue.Text
        Call setGetCurDataPlaceBtn_Click(sender, e)
        'If resistanceCoeffValue.Text <> "" Then
        '    dampingFactor = resistanceCoeffValue.Text / (2 * mass)
        '    dampingFactorLbl.Text = dampingFactor
        '    cyclicFrequency = Math.Sqrt(9.8 / (armLenght / 100))
        '    cyclicFrequency = Math.Sqrt(cyclicFrequency ^ 2 - dampingFactor ^ 2)
        '    oscillationPeriod = (2 * Math.PI) / cyclicFrequency
        'Else
        '    cyclicFrequency = Math.Sqrt(9.8 / (armLenghtValue.Text / 100))
        '    oscillationPeriod = (2 * Math.PI) / cyclicFrequency
        'End If
        'cyclicFrequencyLbl.Text = cyclicFrequency
        'oscillationPeriodLbl.Text = oscillationPeriod


        'bobX = (arm.StartPoint.X + armLenght * Math.Sin(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        'bobY = (arm.StartPoint.Y + armLenght * Math.Cos(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        'bob.Location = New Point(bobX, bobY)
        'arm.EndPoint = New Point(bobX + (bob.Size.Width / 2), bobY + (bob.Size.Width / 2))

        'fullEnergyAtStart = mass * 9.81 * (armLenght / 100) * (1 - Math.Cos(angle * angleDegrToRad))
        'fullEnergyAtStartLbl.Text = fullEnergyAtStart
    End Sub

    Private Sub periodTmr_Tick(sender As Object, e As EventArgs) Handles periodTmr.Tick
        msSW += 1
        If msSW < 10 Then
            mmsSW = "0" & msSW.ToString
        ElseIf msSW < 100 Then
            mmsSW = "0" & msSW.ToString
            mmsSW = msSW.ToString
        Else
            mmsSW = msSW.ToString
        End If
        perTimeLbl.Text = hhSW & ":" & mmSW & ":" & ssSW & ":" & mmsSW
        If msSW = 100 Then
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
            armLenght = Math.Sqrt((arm.EndPoint.X - arm.StartPoint.X) ^ 2 + (arm.EndPoint.Y - arm.StartPoint.Y) ^ 2)
            angle = (Math.Asin((bob.Location.X + (bob.Size.Width / 2) - arm.StartPoint.X) / armLenght)) * angleRadToDegr

            'armLenghtLbl.Text = Math.Sqrt((arm.EndPoint.X - arm.StartPoint.X) ^ 2 + (arm.EndPoint.Y - arm.StartPoint.Y) ^ 2)
            'angleLbl.Text = (Math.Asin((bob.Location.X + (bob.Size.Width / 2) - arm.StartPoint.X) / armLenghtLbl.Text)) * angleRadToDegr
            'angle = angleLbl.Text
            'armLenght = armLenghtLbl.Text
            'angleValue.Text = angleLbl.Text
            'armLenghtValue.Text = armLenghtLbl.Text
            angleValue.Text = angle
            armLenghtValue.Text = armLenght

            Call setGetCurDataPlaceBtn_Click(sender, e)


            'armLenghtLbl.Text = Math.Sqrt((arm.EndPoint.X - arm.StartPoint.X) ^ 2 + (arm.EndPoint.Y - arm.StartPoint.Y) ^ 2)
            'angleLbl.Text = (Math.Asin((bob.Location.X + (bob.Size.Width / 2) - arm.StartPoint.X) / armLenghtLbl.Text)) * angleRadToDegr

            'angle = angleLbl.Text
            'armLenght = armLenghtLbl.Text
            'angleValue.Text = angleLbl.Text
            'armLenghtValue.Text = armLenghtLbl.Text

            'If resistanceCoeffValue.Text <> "" Then
            '    dampingFactor = resistanceCoeffValue.Text / (2 * mass)
            '    dampingFactorLbl.Text = dampingFactor
            '    cyclicFrequency = Math.Sqrt(9.81 / (armLenghtLbl.Text / 100))
            '    cyclicFrequency = Math.Sqrt(cyclicFrequency ^ 2 - dampingFactor ^ 2)
            '    oscillationPeriod = (2 * Math.PI) / cyclicFrequency
            'Else
            '    cyclicFrequency = Math.Sqrt(9.81 / (armLenghtLbl.Text / 100))
            '    oscillationPeriod = (2 * Math.PI) / cyclicFrequency
            'End If
            'cyclicFrequencyLbl.Text = cyclicFrequency
            'oscillationPeriodLbl.Text = oscillationPeriod

            'fullEnergyAtStart = mass * 9.81 * (armLenght / 100) * (1 - Math.Cos(angle * angleDegrToRad))
            'fullEnergyAtStartLbl.Text = fullEnergyAtStart

        End If
    End Sub

    Private Sub getCurDataPlaceBtn_Click(sender As Object, e As EventArgs) Handles getCurDataPlaceBtn.Click
        armLenghtLbl.Text = Math.Sqrt((arm.EndPoint.X - arm.StartPoint.X) ^ 2 + (arm.EndPoint.Y - arm.StartPoint.Y) ^ 2)
        angleLbl.Text = (Math.Asin((bob.Location.X + (bob.Size.Width / 2) - arm.StartPoint.X) / armLenghtLbl.Text)) * angleRadToDegr
    End Sub

    Private Sub setGetCurDataPlaceBtn_Click(sender As Object, e As EventArgs) Handles setGetCurDataPlaceBtn.Click
        Dim resistanceCoeff As Double
        aVel = 0
        aAcc = 0
        mass = massValue.Text
        hSW = 0
        mSW = 0
        sSW = 0
        msSW = 0
        hhSW = "00"
        mmSW = "00"
        ssSW = "00"
        mmsSW = "00"
        numberTotalVibrations = 0
        perFlag2 = True
        perFlag = True
        perFlag3 = False
        'armLenghtLbl.Text = Math.Sqrt((arm.EndPoint.X - arm.StartPoint.X) ^ 2 + (arm.EndPoint.Y - arm.StartPoint.Y) ^ 2)
        'angleLbl.Text = (Math.Asin((bob.Location.X + (bob.Size.Width / 2) - arm.StartPoint.X) / armLenghtLbl.Text)) * angleRadToDegr

        'angle = angleLbl.Text
        'armLenght = armLenghtLbl.Text
        'angleValue.Text = angleLbl.Text
        'armLenghtValue.Text = armLenghtLbl.Text

        If resistanceCoeffValue.Text <> "" Then
            resistanceCoeff = resistanceCoeffValue.Text
            resistanceCoeff = resistanceCoeff / 4999.5
            dampingFactor = resistanceCoeff / (2 * mass)
            dampingFactorLbl.Text = dampingFactor
            'cyclicFrequency = Math.Sqrt(9.8 / (armLenghtLbl.Text / 100))
            cyclicFrequency = Math.Sqrt(((Math.Sqrt(g / (armLenght / 100))) ^ 2) - ((dampingFactor) ^ 2))
            oscillationPeriod = (2 * Math.PI) / cyclicFrequency

        Else
            dampingFactor = 0
            cyclicFrequency = Math.Sqrt(g / (armLenght / 100))
            oscillationPeriod = (2 * Math.PI) / cyclicFrequency
        End If

        cyclicFrequencyLbl.Text = cyclicFrequency
        oscillationPeriodLbl.Text = oscillationPeriod

        bobX = (arm.StartPoint.X + armLenght * Math.Sin(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        bobY = (arm.StartPoint.Y + armLenght * Math.Cos(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        bob.Location = New Point(bobX, bobY)
        arm.EndPoint = New Point(bobX + (bob.Size.Width / 2), bobY + (bob.Size.Width / 2))

        fullEnergyAtStart = mass * g * (armLenght / 100) * (1 - Math.Cos(angle * angleDegrToRad))
        fullEnergyAtStartLbl.Text = fullEnergyAtStart
        firstAngle = (Math.Asin((bob.Location.X + (bob.Size.Width / 2) - arm.StartPoint.X) / (armLenght))) * angleRadToDegr
        firstAngleLbl.Text = firstAngle
        fullEnergy = fullEnergyAtStart

    End Sub

    Private Sub startBtn_Click(sender As Object, e As EventArgs) Handles startBtn.Click
        If startBtn.Text = "Start" Then
            moveTmr.Start()
            periodTmr.Start()
            T = 0
            'aVel = 0
            'aAcc = 0
            'mass = 1
            'hSW = 0
            'mSW = 0
            'sSW = 0
            'msSW = 0
            'hhSW = "00"
            'mmSW = "00"
            'ssSW = "00"
            'mmsSW = "00"

            'fullEnergyAtStartLbl.Text = (mass * (g / (armLenght / 100)) * (angle * angleRadToDegr * (armLenght / 100)) ^ 2) / 2 * (armLenght / 100)
            'fullEnergyAtStart = mass * 9.81 * (armLenght / 100) * (1 - Math.Cos(angle * angleDegrToRad))
            'fullEnergyAtStartLbl.Text = fullEnergyAtStart
            ' firstAngle = (Math.Asin((bob.Location.X + (bob.Size.Width / 2) - arm.StartPoint.X) / (armLenght))) * angleRadToDegr
            startBtn.Text = "Stop"
            'fullEnergy = fullEnergyAtStart
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
        Me.SetStyle(ControlStyles.UserPaint, True)
        Me.SetStyle(ControlStyles.AllPaintingInWmPaint, True)
        Me.SetStyle(ControlStyles.DoubleBuffer, True)
        moveTmr.Enabled = False
        bob.Cursor = Cursors.SizeAll
        T = 0

    End Sub

    Private Sub moveTmr_Tick(sender As Object, e As EventArgs) Handles moveTmr.Tick
        Dim kineticEnergy, potentialEnergy, fullE, velEnVal, a, omega, vT, xT, curT As Double
        Dim i As Integer
        Const deltaT As Double = 0.1
        curT += deltaT
        pEn.Text = curT
        'angle = (Math.Asin((xMax * armLenght) / (bobX)))
        angle = -(omega ^ 2) * xMax * Math.Sin(omega * curT + angleAtStart)
        crAngle.Text = angle
        '    Dim kineticEnergy, potentialEnergy, kineticEnergyEx, potentialEnergyEx, resistanceCoeff, velEnVal As Double
        '    Dim i As Integer

        '    aAcc = -((g / armLenght) * (angle * armLenght))
        '    aVel += aAcc * dt
        '    aVel *= 1 - dampingFactor
        '    'curVelLbl.Text = (((((aVel / 12.84034) * 1.01261) / 1.87) * 1.25708) / 1.081) * 1.0635
        '    curVelLbl.Text = aVel * 0.052011
        '    angle += (aVel / (Math.Sqrt(g / armLenght) * armLenght))
        '    T += dt
        '    Label13.Text = T

        '    'aAcc = -((g / armLenght) * (angle * armLenght))
        '    'aVel += aAcc * dt
        '    'If resistanceCoeffValue.Text <> "" Then
        '    '    aVel *= 1 - resistanceCoeffValue.Text
        '    'End If
        '    'curVelLbl.Text = (aVel / 12.84034) * 1.01261
        '    'angle += (aVel / (Math.Sqrt(g / armLenght) * armLenght))
        '    'T += dt
        '    'Label13.Text = T


        '    potentialEnergy = mass * 9.81 * (armLenght / 100) * (1 - Math.Cos(angle * angleDegrToRad))
        '    potentialEnergyLbl.Text = potentialEnergy
        '    'kineticEnergy = fullEnergy - potentialEnergy
        '    kineticEnergy = (mass * (aVel * 0.052011) ^ 2) / 2
        '    kineticEnergyLbl.Text = kineticEnergy
        '    velEnVal = Math.Sqrt((kineticEnergy * 2) / (mass))
        '    velEnValue.Text = velEnVal
        '    curFullEnergyLbl.Text = potentialEnergy + kineticEnergy
        '    fullEnergy = potentialEnergy + kineticEnergy
        '    If Math.Round(fullEnergy, 4) = 0 Then
        '        moveTmr.Stop()
        '        periodTmr.Stop()
        '        moveTmr.Stop()
        '        periodTmr.Stop()
        '        hSW = 0
        '        mSW = 0
        '        sSW = 0
        '        msSW = 0
        '        perTimeLbl.Text = hhSW & ":" & mmSW & ":" & ssSW & ":" & mmsSW
        '        startBtn.Text = "Start"
        '    End If

        '    ' kineticEnergyEx = (mass * velEnValue.Text ^ 2) / 2
        '    'kineticEnergyLbl.Text = kineticEnergy
        '    'potentialEnergyEx = (mass * (Math.Sqrt(g / (armLenght / 100)) ^ 2) * (angle * angleRadToDegr * (armLenght / 100)) ^ 2) / (2 * (armLenght / 100))
        '    'potentialEnergyLbl.Text = potentialEnergy
        '    'curFullEnergyLbl.Text = kineticEnergyEx + potentialEnergyEx
        '    'fullEnergyLbl.Text = fullEnergy
        '    curAngleLbl.Text = angle
        '    bobX = (arm.StartPoint.X + armLenght * Math.Sin(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        '    bobY = (arm.StartPoint.Y + armLenght * Math.Cos(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        '    bob.Location = New Point(bobX, bobY)
        '    arm.EndPoint = New Point(bobX + (bob.Size.Width / 2), bobY + (bob.Size.Width / 2))


        '    numberTotalVibrations = ((T * 3.13) / oscillationPeriod) * 1.0577
        '    'Label19.Text = Math.Round(numberTotalVibrations, 0)
        '    Label19.Text = Math.Round(numberTotalVibrations, 1).ToString().Substring(0, 1)




        '    'aAcc = -((g / armLenght) * Math.Sin(angle * angleDegrToRad)
        '    'angle +=aVel*dt 
        '    'aVel += aAcc * dt


        '    'If prewAngle < 0 And angle >= 0 And perFlag = True Then
        '    '    perFlag = False
        '    '    numberTotalVibrations = numberTotalVibrations + 1
        '    '    Label10.Text = numberTotalVibrations
        '    '    Label19.Text = perFlag
        '    'Else
        '    '    perFlag = False
        '    '    Label19.Text = perFlag
        '    'End If

        '    If Math.Round(firstAngle, 2) = Math.Round(angle, 2) And aVel <> 0 Then
        '            'moveTmr.Stop()
        '            oscillationPeriodPrewLbl.Text = perTimeLbl.Text
        '            'periodTmr.Stop()
        '        End If
    End Sub
End Class
