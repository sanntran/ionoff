package net.ionoff.webhook.controller;

public class AccentRemover {

        private static char[] SPECIAL_CHARACTERS =
                {
                ' ', '!', '"', '#', '$',
                '%', '*', '+', ',', ':',
                '<', '=', '>', '?', '@',
                '[', '\\', ']', '^', '`',
                '|', '~',
                'A', 'À', 'Á', 'Ả', 'Ạ', 'Ã',
                'Ă', 'Ằ', 'Ắ', 'Ẳ', 'Ặ', 'Ẵ',
                'Â', 'Ầ', 'Ấ', 'Ẩ', 'Ậ', 'Ẫ',
                'E', 'È', 'É', 'Ẻ', 'Ẹ', 'Ẽ',
                'Ê', 'Ề', 'Ế', 'Ể', 'Ệ', 'Ễ',
                'I', 'Ì', 'Í', 'Ỉ', 'Ị', 'Ĩ',
                'O', 'Ò', 'Ó', 'Ỏ', 'Ọ', 'Õ',
                'Ô', 'Ồ', 'Ố', 'Ổ', 'Ộ', 'Ỗ',
                'Ơ', 'Ờ', 'Ớ', 'Ở', 'Ợ', 'Ỡ',
                'U', 'Ù', 'Ú', 'Ủ', 'Ụ', 'Ũ',
                'Ư', 'Ừ', 'Ứ', 'Ử', 'Ự', 'Ữ',
                'Y', 'Ỳ', 'Ý', 'Ỷ', 'Ỵ', 'Ỹ',
                'Đ',
                'a', 'à', 'á', 'ả', 'ạ', 'ã',
                'ă', 'ằ', 'ắ', 'ẳ', 'ặ', 'ẵ',
                'â', 'ầ', 'ấ', 'ẩ', 'ậ', 'ẫ',
                'e', 'è', 'é', 'ẻ', 'ẹ', 'ẽ',
                'ê', 'ề', 'ế', 'ể', 'ệ', 'ễ',
                'i', 'ì', 'í', 'ỉ', 'ị', 'ĩ',
                'o', 'ò', 'ó', 'ỏ', 'ọ', 'õ',
                'ô', 'ồ', 'ố', 'ổ', 'ộ', 'ỗ',
                'ơ', 'ờ', 'ớ', 'ở', 'ợ', 'ỡ',
                'u', 'ù', 'ú', 'ủ', 'ụ', 'ũ',
                'ư', 'ừ', 'ứ', 'ử', 'ự', 'ữ',
                'y', 'ỳ', 'ý', 'ỷ', 'ỵ', 'ỹ',
                'đ'};

        private static char[] REPLACEMENTS = {
                '-', '-', '-', '-', '-',
                '-', '-', '-', '-', '-',
                '-', '-', '-', '-', '-',
                '-', '-', '-', '-', '-',
                '-', '-',
                'A', 'A', 'A', 'A', 'A', 'A',
                'A', 'A', 'A', 'A', 'A', 'A',
                'A', 'A', 'A', 'A', 'A', 'A',
                'E', 'E', 'E', 'E', 'E', 'E',
                'E', 'E', 'E', 'E', 'E', 'E',
                'I', 'I', 'I', 'I', 'I', 'I',
                'O', 'O', 'O', 'O', 'O', 'O',
                'O', 'O', 'O', 'O', 'O', 'O',
                'O', 'O', 'O', 'O', 'O', 'O',
                'U', 'U', 'U', 'U', 'U', 'U',
                'U', 'U', 'U', 'U', 'U', 'U',
                'Y', 'Y', 'Y', 'Y', 'Y', 'Y',
                'D',
                'a', 'a', 'a', 'a', 'a', 'a',
                'a', 'a', 'a', 'a', 'a', 'a',
                'a', 'a', 'a', 'a', 'a', 'a',
                'e', 'e', 'e', 'e', 'e', 'e',
                'e', 'e', 'e', 'e', 'e', 'e',
                'i', 'i', 'i', 'i', 'i', 'i',
                'o', 'o', 'o', 'o', 'o', 'o',
                'o', 'o', 'o', 'o', 'o', 'o',
                'o', 'o', 'o', 'o', 'o', 'o',
                'u', 'u', 'u', 'u', 'u', 'u',
                'u', 'u', 'u', 'u', 'u', 'u',
                'y', 'y', 'y', 'y', 'y', 'y',
                'd'
                };

        public static String toUrlFriendly(String s) {
                int maxLength = Math.min(s.length(), 236);
                char[] buffer = new char[maxLength];
                int n = 0;
                for (int i = 0; i < maxLength; i++) {
                        char ch = s.charAt(i);
                        buffer[n] = removeAccent(ch);
                        // skip not printable characters
                        if (buffer[n] > 31) {
                                n++;
                        }
                }
                // skip trailing slashes
                while (n > 0 && buffer[n - 1] == '/') {
                        n--;
                }
                return String.valueOf(buffer, 0, n);
        }

        public static char removeAccent(char ch) {
                int index = findCharIndex(ch);
                if (index >= 0) {
                        return REPLACEMENTS[index];
                }
                return ch;
        }

        private static int findCharIndex(char ch) {
                for (int i = 0; i < SPECIAL_CHARACTERS.length; i++) {
                        if (SPECIAL_CHARACTERS[i] == ch) {
                                return i;
                        }
                }
                return -1;
        }
        
        public static String removeAccent(String s) {
                StringBuilder sb = new StringBuilder(s);
                for (int i = 0; i < sb.length(); i++) {
                        sb.setCharAt(i, removeAccent(sb.charAt(i)));
                }
                return sb.toString();
        }
}