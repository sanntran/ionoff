package net.ionoff.broker.http;

import net.ionoff.broker.tcp.ClientException;
import net.ionoff.broker.tcp.ContentType;
import net.ionoff.broker.tcp.ServerException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.net.ssl.SSLException;
import java.io.*;
import java.net.SocketException;
import java.net.URLDecoder;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.*;

public class HttpRequest {

    private static final Logger LOGGER = LoggerFactory.getLogger(HttpRequest.class);

    private static final int REQUEST_BUFFER_LEN = 512;

    public static final int BUFSIZE = 8192;

    private final BufferedInputStream inputStream;

    private int splitbyte;

    private int rlen;

    private String uri;

    private HttpMethod method;

    private Map<String, List<String>> parms;

    private Map<String, String> headers;

    private String queryParameterString;

    private String protocolVersion;

    private String body;

    public HttpRequest(InputStream inputStream) {
        this.headers = new HashMap<>();
        this.inputStream = new BufferedInputStream(inputStream, HttpRequest.BUFSIZE);
    }

    private void decodeHeader(BufferedReader in, Map<String, String> pre, Map<String, List<String>> parms, Map<String, String> headers) {
        try {
            // Read the request line
            String inLine = in.readLine();
            if (inLine == null || !inLine.contains("HTTP")) {
                throw new HttpException(inLine);
            }
            StringTokenizer st = new StringTokenizer(inLine);
            if (!st.hasMoreTokens()) {
               throw new ClientException(HttpStatus.BAD_REQUEST, "Syntax error. Missing method");
            }
            pre.put("method", st.nextToken());
            if (!st.hasMoreTokens()) {
               throw new ClientException(HttpStatus.BAD_REQUEST, "Syntax error. Missing URI");
            }
            String uri = st.nextToken();
            // Decode parameters from the URI
            int qmi = uri.indexOf('?');
            if (qmi >= 0) {
                decodeParms(uri.substring(qmi + 1), parms);
                uri = decodePercent(uri.substring(0, qmi));
            } else {
                uri = decodePercent(uri);
            }
            // If there's another token, its protocol version,
            // followed by HTTP headers.
            // NOTE: this now forces header names lower case since they are
            // case insensitive and vary by client.
            if (st.hasMoreTokens()) {
                protocolVersion = st.nextToken();
            } else {
                protocolVersion = "HTTP/1.1";
                LOGGER.info("No protocol version specified, strange. Assuming HTTP/1.1.");
            }
            String line = in.readLine();
            while (line != null && !line.trim().isEmpty()) {
                int p = line.indexOf(':');
                if (p >= 0) {
                    headers.put(line.substring(0, p).trim().toLowerCase(Locale.US), line.substring(p + 1).trim());
                }
                line = in.readLine();
            }
            pre.put("uri", uri);
        } catch (IOException ioe) {
            throw new ServerException("Internal Server Error " + ioe.getMessage(), ioe);
        }
    }

    public static String decodePercent(String str) {
        String decoded = null;
        try {
            decoded = URLDecoder.decode(str, "UTF8");
        } catch (UnsupportedEncodingException ignored) {
           LOGGER.warn("Encoding not supported, ignored", ignored);
        }
        return decoded;
    }

    private int scipOverNewLine(byte[] partHeaderBuff, int index) {
        while (partHeaderBuff[index] != '\n') {
            index++;
        }
        return ++index;
    }
    private void decodeParms(String parms, Map<String, List<String>> p) {
        if (parms == null) {
            this.queryParameterString = "";
            return;
        }

        this.queryParameterString = parms;
        StringTokenizer st = new StringTokenizer(parms, "&");
        while (st.hasMoreTokens()) {
            String e = st.nextToken();
            int sep = e.indexOf('=');
            String key = null;
            String value = null;

            if (sep >= 0) {
                key = decodePercent(e.substring(0, sep)).trim();
                value = decodePercent(e.substring(sep + 1));
            } else {
                key = decodePercent(e).trim();
                value = "";
            }

            List<String> values = p.get(key);
            if (values == null) {
                values = new ArrayList<String>();
                p.put(key, values);
            }

            values.add(value);
        }
    }

    public void readRequest() throws IOException {
        // Read the first 8192 bytes.
        // The full header should fit in here.
        // Apache's default header limit is 8KB.
        // Do NOT assume that a single read will get the entire header
        // at once!
        byte[] buf = new byte[HttpRequest.BUFSIZE];
        this.splitbyte = 0;
        this.rlen = 0;

        int read = -1;
        this.inputStream.mark(HttpRequest.BUFSIZE);
        try {
            read = this.inputStream.read(buf, 0, HttpRequest.BUFSIZE);
        } catch (SSLException e) {
            throw e;
        } catch (IOException e) {
            throw new SocketException("Server shutdown");
        }
        if (read == -1) {
            // socket was been closed
            throw new SocketException("Server shutdown");
        }
        while (read > 0) {
            this.rlen += read;
            this.splitbyte = findHeaderEnd(buf, this.rlen);
            if (splitbyte == 0) {
                for (int i = 0; i < read; i++) {
                    if ((char) buf[i] == ':') {
                       splitbyte = 2;
                       break;
                    }
                }
            }
            if (this.splitbyte > 0) {
                break;
            }
            read = this.inputStream.read(buf, this.rlen, HttpRequest.BUFSIZE - this.rlen);
        }
        if (this.splitbyte < this.rlen) {
            this.inputStream.reset();
            this.inputStream.skip(this.splitbyte);
        }
        this.parms = new HashMap<>();
        if (null == this.headers) {
            this.headers = new HashMap<>();
        } else {
            this.headers.clear();
        }
        BufferedReader hin = new BufferedReader(
                new InputStreamReader(new ByteArrayInputStream(buf, 0, this.rlen)));
        // Decode the header into parms and header java properties
        Map<String, String> pre = new HashMap<>();
        decodeHeader(hin, pre, this.parms, this.headers);

        this.method = HttpMethod.lookup(pre.get("method"));
        if (this.method == null) {
            throw new ClientException(HttpStatus.BAD_REQUEST,
                    "Syntax error. HTTP verb " + pre.get("method") + " unhandled.");
        }
        this.uri = pre.get("uri");
        parseBody();
    }
    private int findHeaderEnd(final byte[] buf, int rlen) {
        int splitbyte = 0;
        while (splitbyte + 1 < rlen) {

            // RFC2616
            if (buf[splitbyte] == '\r' && buf[splitbyte + 1] == '\n' && splitbyte + 3 < rlen && buf[splitbyte + 2] == '\r' && buf[splitbyte + 3] == '\n') {
                return splitbyte + 4;
            }

            // tolerance
            if (buf[splitbyte] == '\n' && buf[splitbyte + 1] == '\n') {
                return splitbyte + 2;
            }
            splitbyte++;
        }
        return 0;
    }

    public final Map<String, String> getHeaders() {
        return this.headers;
    }

   
    public final InputStream getInputStream() {
        return this.inputStream;
    }

   
    public final HttpMethod getMethod() {
        return this.method;
    }

    public final Map<String, List<String>> getParameters() {
        return this.parms;
    }

    public String getQueryParameterString() {
        return this.queryParameterString;
    }

    public final String getUri() {
        return this.uri;
    }

    public long getBodySize() {
        if (this.headers.containsKey("content-length")) {
            return Long.parseLong(this.headers.get("content-length"));
        } else if (this.splitbyte < this.rlen) {
            return this.rlen - this.splitbyte;
        }
        return 0;
    }

    public void parseBody() throws IOException {
        RandomAccessFile randomAccessFile = null;
        long size = getBodySize();
        ByteArrayOutputStream baos = null;
        DataOutput requestDataOutput = null;

        baos = new ByteArrayOutputStream();
        requestDataOutput = new DataOutputStream(baos);

        // Read all the body and write it to request_data_output
        byte[] buf = new byte[REQUEST_BUFFER_LEN];
        while (this.rlen >= 0 && size > 0) {
            this.rlen = this.inputStream.read(buf, 0, (int) Math.min(size, REQUEST_BUFFER_LEN));
            size -= this.rlen;
            if (this.rlen > 0) {
                requestDataOutput.write(buf, 0, this.rlen);
            }
        }
        ByteBuffer fbuf = null;
        if (baos != null) {
            fbuf = ByteBuffer.wrap(baos.toByteArray(), 0, baos.size());
        } else {
            fbuf = randomAccessFile.getChannel().map(FileChannel.MapMode.READ_ONLY, 0,
                    randomAccessFile.length());
            randomAccessFile.seek(0);
        }
        // If the method is POST, there may be parameters
        // in data section, too, read it:
        if (HttpMethod.POST.equals(this.method) || HttpMethod.PUT.equals(this.method)) {
            ContentType contentType = new ContentType(this.headers.get("content-type"));
            if (contentType.isMultipart()) {
                throw new ServerException("Not support content type multipart/form-data");
            } else {
                byte[] postBytes = new byte[fbuf.remaining()];
                fbuf.get(postBytes);
                String postLine = new String(postBytes, contentType.getEncoding()).trim();
                // Handle application/x-www-form-urlencoded
                if ("application/x-www-form-urlencoded".equalsIgnoreCase(contentType.getContentType())) {
                    decodeParms(postLine, this.parms);
                } else if (postLine.length() != 0) {
                    body = postLine;
                }
            }
        }
    }

    public String getBody() {
        return body;
    }
}
