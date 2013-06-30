Imports VBF.Compilers.Scanners
Imports System.Reflection
Imports System.Text

Class FormatHelper
    Private m_indentation As Integer = 0
    Private m_builder As StringBuilder

    Sub New()
        m_builder = New StringBuilder()
    End Sub

    Sub BeginList()
        AppendIndentation()
        m_builder.AppendLine("["c)
        Indent()
    End Sub

    Sub EndList()
        Dedent()
        AppendIndentation()
        m_builder.AppendLine("]"c)
    End Sub

    Private Sub AppendIndentation()
        If m_indentation > 0 Then
            m_builder.Append(New String(" "c, m_indentation))
        End If
    End Sub

    Sub Indent()
        m_indentation += 4
    End Sub

    Sub Dedent()
        m_indentation -= 4
        If m_indentation < 0 Then m_indentation = 0
    End Sub

    Sub AddListSeperator()
        AppendIndentation()
        m_builder.AppendLine(","c)
    End Sub

    Sub AddString(str As String)
        AppendIndentation()
        m_builder.AppendLine(str)
    End Sub

    Sub BeginBlock()
        AppendIndentation()
        m_builder.AppendLine("{"c)
        Indent()
    End Sub

    Sub EndBlock()
        Dedent()
        AppendIndentation()
        m_builder.AppendLine("}"c)
    End Sub

    Sub AddStringNoReturn(str As String)
        AppendIndentation()
        m_builder.Append(str)
    End Sub

    Sub AppendString(str As String)
        m_builder.Append(str)
    End Sub

    Sub AppendLineString(str As String)
        m_builder.AppendLine(str)
    End Sub

    Public Overrides Function ToString() As String
        Return m_builder.ToString()
    End Function

    Public Sub ObjectToString(obj As Object)
        If obj Is Nothing Then Return

        Dim nodeType = obj.GetType()
        Me.AddString(nodeType.Name)
        Me.BeginBlock()

        Dim props = nodeType.GetProperties()

        For Each field In props
            If field.PropertyType.Equals(GetType(UnifiedIdentifer)) Then
                Me.AddStringNoReturn(field.Name)
                Me.AppendString(":")
                Dim id = DirectCast(field.GetValue(obj), UnifiedIdentifer)
                If id IsNot Nothing Then
                    Me.AppendLineString(id.Identifier)
                Else
                    Me.AppendLineString(Nothing)
                End If
            ElseIf GetType(SyntaxTreeNode).IsAssignableFrom(field.PropertyType) OrElse
                GetType(SyntaxTreeData).IsAssignableFrom(field.PropertyType) Then
                'field is syntax node
                Me.AddStringNoReturn(field.Name)
                Me.AppendLineString(":")

                ObjectToString(field.GetValue(obj))
            ElseIf GetType(IEnumerable(Of SyntaxTreeNode)).IsAssignableFrom(field.PropertyType) OrElse
                GetType(IEnumerable(Of SyntaxTreeData)).IsAssignableFrom(field.PropertyType) Then
                'field is syntax node list
                Me.AddStringNoReturn(field.Name)
                Me.AppendLineString(":")

                EnumerableToString(TryCast(field.GetValue(obj), IEnumerable))
            ElseIf field.PropertyType.Equals(GetType(LexemeValue)) Then
                Me.AddStringNoReturn(field.Name)
                Me.AppendString(":")
                Dim z = DirectCast(field.GetValue(obj), LexemeValue)
                If z IsNot Nothing Then
                    Me.AppendLineString(z.Content)
                Else
                    Me.AppendLineString(Nothing)
                End If
            ElseIf field.PropertyType.Equals(GetType(ExpressionOp)) Then
                Me.AddStringNoReturn(field.Name)
                Me.AppendString(":")
                Dim op = DirectCast(field.GetValue(obj), ExpressionOp)
                Me.AppendLineString(op.ToString)
            End If
        Next

        Me.EndBlock()
    End Sub

    Public Sub EnumerableToString(enumerable As IEnumerable)
        If enumerable Is Nothing Then Return

        Me.BeginList()
        For Each item As Object In enumerable
            ObjectToString(item)
            Me.AddListSeperator()
        Next
        Me.EndList()
    End Sub
End Class