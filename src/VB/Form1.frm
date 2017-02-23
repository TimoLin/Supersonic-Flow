VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "有旋特征线法超音速流场计算程序"
   ClientHeight    =   6315
   ClientLeft      =   4620
   ClientTop       =   2235
   ClientWidth     =   12810
   LinkTopic       =   "Form1"
   ScaleHeight     =   6315
   ScaleWidth      =   12810
   Begin VB.PictureBox Picture1 
      Height          =   4335
      Left            =   5400
      Picture         =   "Form1.frx":0000
      ScaleHeight     =   4275
      ScaleWidth      =   7035
      TabIndex        =   14
      Top             =   360
      Width           =   7095
   End
   Begin VB.CommandButton Command2 
      Caption         =   "退出"
      Height          =   495
      Left            =   3240
      TabIndex        =   13
      Top             =   5280
      Width           =   975
   End
   Begin VB.TextBox Text5 
      Alignment       =   2  'Center
      Height          =   495
      Left            =   3120
      TabIndex        =   12
      Top             =   3720
      Width           =   1215
   End
   Begin VB.TextBox Text4 
      Alignment       =   2  'Center
      Height          =   450
      Left            =   3120
      TabIndex        =   8
      Top             =   2880
      Width           =   1215
   End
   Begin VB.TextBox Text3 
      Alignment       =   2  'Center
      Height          =   450
      Left            =   3120
      TabIndex        =   6
      Top             =   2040
      Width           =   1215
   End
   Begin VB.TextBox Text2 
      Alignment       =   2  'Center
      Height          =   450
      Left            =   3120
      TabIndex        =   4
      Top             =   1200
      Width           =   1215
   End
   Begin VB.TextBox Text1 
      Alignment       =   2  'Center
      Height          =   450
      Left            =   3120
      TabIndex        =   2
      Top             =   360
      Width           =   1215
   End
   Begin VB.CommandButton Command1 
      Caption         =   "开始计算"
      Height          =   495
      Left            =   1200
      TabIndex        =   0
      Top             =   5280
      Width           =   1095
   End
   Begin VB.Label Label7 
      Alignment       =   2  'Center
      Height          =   375
      Left            =   3120
      TabIndex        =   11
      Top             =   4680
      Width           =   1215
   End
   Begin VB.Label Label6 
      Alignment       =   2  'Center
      Caption         =   "壁面静压恢复为来流静压的位置X"
      Height          =   450
      Left            =   360
      TabIndex        =   10
      Top             =   4560
      Width           =   1575
   End
   Begin VB.Label Label5 
      Alignment       =   2  'Center
      Caption         =   "半径Ye（米）"
      Height          =   450
      Left            =   360
      TabIndex        =   9
      Top             =   3720
      Width           =   1575
   End
   Begin VB.Label Label4 
      Alignment       =   2  'Center
      Caption         =   "总长度Xe（米）"
      Height          =   450
      Left            =   360
      TabIndex        =   7
      Top             =   2880
      Width           =   1575
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      Caption         =   "比例Ya/Ye"
      Height          =   450
      Left            =   360
      TabIndex        =   5
      Top             =   2040
      Width           =   1575
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Caption         =   "角度A(度)"
      Height          =   450
      Left            =   360
      TabIndex        =   3
      Top             =   1200
      Width           =   1575
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "来流马赫数MU"
      Height          =   450
      Left            =   360
      TabIndex        =   1
      Top             =   360
      Width           =   1575
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Declare Sub MAIN Lib "supersonic.dll" (ByRef MU As Single, ByRef TA As Single, ByRef ZETA As Single, ByRef XE As Single, ByRef YE As Single, ByRef Xlocation As Single)

Private Sub Command1_Click()
 Dim MU, TA, ZETA, XE, YE, Xlocation As Single
 
 MU = Text1.Text
 TA = Text2.Text
 ZETA = Text3.Text
 XE = Text4.Text
 YE = Text5.Text
 
 Label7.Caption = ""
 
 Call MAIN(MU, TA, ZETA, XE, YE, Xlocation)
 
 Label7.Caption = Str(Xlocation)
 
 
End Sub

Private Sub Command2_Click()
 End
End Sub
