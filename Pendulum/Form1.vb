Imports Microsoft.VisualBasic.PowerPacks
Public Class Form1
    Dim armLenght, angle, mass, bobX, bobY,  cyclicFrequency, oscillationPeriod, dampingFactor, fullEnergyAtStart, angleAtStart As Double
    Dim omega, theta, dt, x, y, T, L, alpha, sysH As Double

    Dim perFlag, perFlag2, perFlag3 As Boolean
    Const deltaT As Double = 0.1
    Dim FlagB0, FlagM0, FlagB As Boolean
    Dim sSW, mSW, hSW As Integer
    Dim mmsSW, ssSW, mmSW, hhSW As String
    Const angleDegrToRad As Double = (Math.PI / 180)
    Const angleRadToDegr As Double = (180 / Math.PI)
    Const g As Double = 9.81
    Const angleСorrection As Double = 90

    Private Sub newMethodBtn_Click(sender As Object, e As EventArgs) Handles newMethodBtn.Click
        Dim resistanceCoeff As Double
        If newMethodBtn.Text = "Запустить" Then


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
            fullEnergyAtStart = mass * 9.81 * (sysH - (bob.Location.Y / 100))
            fEnStart.Text = fullEnergyAtStart
            TmoveTmrTwo.Start()
            newMethodBtn.Text = "Стоп"
        Else
            newMethodBtn.Text = "Запустить"
            TmoveTmrTwo.Stop()
        End If
    End Sub

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
        V = Math.Sqrt((2 * kineticEnergy) / (mass))
        enVel.Text = V

        bobX = (arm.StartPoint.X + L * Math.Sin(theta)) - (bob.Size.Width / 2)
        bobY = (arm.StartPoint.Y + L * Math.Cos(theta)) - (bob.Size.Width / 2)
        bob.Location = New Point(bobX, bobY)
        arm.EndPoint = New Point(bobX + (bob.Size.Width / 2), bobY + (bob.Size.Width / 2))


    End Sub

    Private Sub setBtn_Click(sender As Object, e As EventArgs) Handles setBtn.Click
        angle = angleValue.Text
        armLenght = armLenghtValue.Text
        Call setGetCurDataPlaceBtn_Click(sender, e)
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
            angleValue.Text = angle
            armLenghtValue.Text = armLenght
            Call setGetCurDataPlaceBtn_Click(sender, e)

        End If
    End Sub

    Private Sub getCurDataPlaceBtn_Click(sender As Object, e As EventArgs) Handles getCurDataPlaceBtn.Click
        armLenghtLbl.Text = Math.Sqrt((arm.EndPoint.X - arm.StartPoint.X) ^ 2 + (arm.EndPoint.Y - arm.StartPoint.Y) ^ 2)
        angleLbl.Text = (Math.Asin((bob.Location.X + (bob.Size.Width / 2) - arm.StartPoint.X) / armLenghtLbl.Text)) * angleRadToDegr
    End Sub

    Private Sub setGetCurDataPlaceBtn_Click(sender As Object, e As EventArgs) Handles setGetCurDataPlaceBtn.Click
        Dim resistanceCoeff As Double
        bobX = (arm.StartPoint.X + armLenght * Math.Sin(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        bobY = (arm.StartPoint.Y + armLenght * Math.Cos(angle * angleDegrToRad)) - (bob.Size.Width / 2)
        bob.Location = New Point(bobX, bobY)
        arm.EndPoint = New Point(bobX + (bob.Size.Width / 2), bobY + (bob.Size.Width / 2))
    End Sub
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.SetStyle(ControlStyles.UserPaint, True)
        Me.SetStyle(ControlStyles.AllPaintingInWmPaint, True)
        Me.SetStyle(ControlStyles.DoubleBuffer, True)
        bob.Cursor = Cursors.SizeAll
        T = 0
    End Sub
End Class
