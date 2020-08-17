using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace FSharpLintVs
{
    /// <summary>
    /// Interaction logic for SuggestionPreview.xaml
    /// </summary>
    public partial class SuggestionPreview : UserControl
    {
        protected SuggestionPreview()
        {
            InitializeComponent();
        }

        public SuggestionPreview(LintError lintError) : this()
        {
            this.DataContext = lintError;
        }
    }
}
