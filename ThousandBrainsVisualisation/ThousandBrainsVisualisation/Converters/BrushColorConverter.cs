using System.Globalization;
using System.Windows.Data;
using System.Windows.Media;

namespace ThousandBrainsVisualisation.Converters
{
    public class BrushColorConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if ((bool)value)
            {
                return Colors.Green;
            }
            return Colors.Red;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }
}
