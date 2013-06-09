Imports System.Text
Imports System.Reflection
Imports VBF.Compilers.Scanners

Public MustInherit Class SyntaxTreeNode

    Private Class FormatHelper
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
    End Class

    ''' <summary>
    ''' Dump the syntax tree to a string representation
    ''' </summary>
    Public Overrides Function ToString() As String
        Dim helper As New FormatHelper
        ObjectToString(Me, helper)

        Return helper.ToString()
    End Function

    Private Shared Sub ObjectToString(obj As SyntaxTreeNode, helper As FormatHelper)
        If obj Is Nothing Then Return

        Dim nodeType = obj.GetType()
        helper.AddString(nodeType.Name)
        helper.BeginBlock()

        Dim fields = nodeType.GetFields(BindingFlags.NonPublic Or BindingFlags.Instance)

        For Each field In fields
            If field.FieldType.Equals(GetType(UnifiedIdentifer)) Then
                helper.AddStringNoReturn(field.Name)
                helper.AppendString(":")
                Dim id = DirectCast(field.GetValue(obj), UnifiedIdentifer)
                If id IsNot Nothing Then
                    helper.AppendLineString(id.Identifier)
                Else
                    helper.AppendLineString(Nothing)
                End If
            ElseIf GetType(SyntaxTreeNode).IsAssignableFrom(field.FieldType) Then
                'field is syntax node
                helper.AddStringNoReturn(field.Name)
                helper.AppendLineString(":")

                ObjectToString(TryCast(field.GetValue(obj), SyntaxTreeNode), helper)
            ElseIf GetType(IEnumerable(Of SyntaxTreeNode)).IsAssignableFrom(field.FieldType) Then
                'field is syntax node list
                helper.AddStringNoReturn(field.Name)
                helper.AppendLineString(":")

                EnumerableToString(TryCast(field.GetValue(obj), IEnumerable), helper)
            ElseIf field.FieldType.Equals(GetType(LexemeValue)) Then
                helper.AddStringNoReturn(field.Name)
                helper.AppendString(":")
                Dim z = DirectCast(field.GetValue(obj), LexemeValue)
                If z IsNot Nothing Then
                    helper.AppendLineString(z.Content)
                Else
                    helper.AppendLineString(Nothing)
                End If
            ElseIf field.FieldType.Equals(GetType(ExpressionOp)) Then
                helper.AddStringNoReturn(field.Name)
                helper.AppendString(":")
                Dim op = DirectCast(field.GetValue(obj), ExpressionOp)
                helper.AppendLineString(op.ToString)
            End If
        Next

        helper.EndBlock()
    End Sub

    Private Shared Sub EnumerableToString(enumerable As IEnumerable, helper As FormatHelper)
        If enumerable Is Nothing Then Return

        helper.BeginList()
        For Each item As SyntaxTreeNode In enumerable
            ObjectToString(item, helper)
            helper.AddListSeperator()
        Next
        helper.EndList()
    End Sub
End Class
