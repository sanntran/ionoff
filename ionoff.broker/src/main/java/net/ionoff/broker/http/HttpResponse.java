package net.ionoff.broker.http;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.text.SimpleDateFormat;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

public class HttpResponse implements Closeable {

    private static final Logger LOGGER = LoggerFactory.getLogger(HttpResponse.class);
    public static final String MIME_JSON = "application/json";


    private HttpStatus status;

    private String mimeType;

    /**
     * Data of the response, may be null.
     */
    private InputStream data;

    private long contentLength;

    @SuppressWarnings("serial")
    private final Map<String, String> header = new HashMap<String, String>() {

        public String put(String key, String value) {
            lowerCaseHeader.put(key == null ? key : key.toLowerCase(), value);
            return super.put(key, value);
        };
    };

    private final Map<String, String> lowerCaseHeader = new HashMap<String, String>();

    private HttpMethod requestMethod;

    private boolean keepAlive;

    protected HttpResponse(HttpStatus status, String mimeType, InputStream data, long totalBytes) {
        this.status = status;
        this.mimeType = mimeType;
        if (data == null) {
            this.data = new ByteArrayInputStream(new byte[0]);
            this.contentLength = 0L;
        } else {
            this.data = data;
            this.contentLength = totalBytes;
        }
        this.keepAlive = true;
    }

    @Override
    public void close() throws IOException {
        if (this.data != null) {
            this.data.close();
        }
    }

    public void addHeader(String name, String value) {
        this.header.put(name, value);
    }

    public void closeConnection(boolean close) {
        if (close)
            this.header.put("connection", "close");
        else
            this.header.remove("connection");
    }

    public boolean isCloseConnection() {
        return "close".equals(getHeader("connection"));
    }

    public InputStream getData() {
        return this.data;
    }

    public String getHeader(String name) {
        return this.lowerCaseHeader.get(name.toLowerCase());
    }

    public String getMimeType() {
        return this.mimeType;
    }

    public HttpMethod getRequestMethod() {
        return this.requestMethod;
    }

    public HttpStatus getStatus() {
        return this.status;
    }

    public void setKeepAlive(boolean useKeepAlive) {
        this.keepAlive = useKeepAlive;
    }

    public void send(OutputStream outputStream) {
        SimpleDateFormat gmtFrmt = new SimpleDateFormat("E, d MMM yyyy HH:mm:ss 'GMT'", Locale.US);
        gmtFrmt.setTimeZone(TimeZone.getTimeZone("GMT"));
        try {
            if (this.status == null) {
                throw new Error("sendResponse(): HttpStatus can't be null.");
            }
            PrintWriter pw = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outputStream, new ContentType(this.mimeType).getEncoding())), false);
            pw.append("HTTP/1.1 ").append(this.status.getDescription()).append(" \r\n");
            if (this.mimeType != null) {
                printHeader(pw, "Content-Type", this.mimeType);
            }
            if (getHeader("date") == null) {
                printHeader(pw, "Date", gmtFrmt.format(new Date()));
            }
            for (Map.Entry<String, String> entry : this.header.entrySet()) {
                printHeader(pw, entry.getKey(), entry.getValue());
            }
            if (getHeader("connection") == null) {
                printHeader(pw, "Connection", (this.keepAlive ? "keep-alive" : "close"));
            }
            long pending = this.data != null ? this.contentLength : 0;
            pending = sendContentLengthHeaderIfNotAlreadyPresent(pw, pending);
            pw.append("\r\n");
            pw.flush();
            sendBodyWithCorrectEncoding(outputStream, pending);
            outputStream.flush();
        } catch (IOException ioe) {
           LOGGER.error("Could not send response to the connector", ioe);
        }
    }

    @SuppressWarnings("static-method")
    protected void printHeader(PrintWriter pw, String key, String value) {
        pw.append(key).append(": ").append(value).append("\r\n");
    }

    protected long sendContentLengthHeaderIfNotAlreadyPresent(PrintWriter pw, long defaultSize) {
        String contentLengthString = getHeader("content-length");
        long size = defaultSize;
        if (contentLengthString != null) {
            try {
                size = Long.parseLong(contentLengthString);
            } catch (NumberFormatException ex) {
                LOGGER.error("Content-length was no number " + contentLengthString);
            }
        }else{
            pw.print("Content-Length: " + size + "\r\n");
        }
        return size;
    }

    private void sendBodyWithCorrectEncoding(OutputStream outputStream, long pending) throws IOException {
        sendBody(outputStream, pending);
    }

    private void sendBody(OutputStream outputStream, long pending) throws IOException {
        long BUFFER_SIZE = 16 * 1024;
        byte[] buff = new byte[(int) BUFFER_SIZE];
        boolean sendEverything = pending == -1;
        while (pending > 0 || sendEverything) {
            long bytesToRead = sendEverything ? BUFFER_SIZE : Math.min(pending, BUFFER_SIZE);
            int read = this.data.read(buff, 0, (int) bytesToRead);
            if (read <= 0) {
                break;
            }
            try {
                outputStream.write(buff, 0, read);
            } catch (Exception e) {
                if(this.data != null) {
                    this.data.close();
                }
            }
            if (!sendEverything) {
                pending -= read;
            }
        }
    }

    public void setData(InputStream data) {
        this.data = data;
    }

    public void setMimeType(String mimeType) {
        this.mimeType = mimeType;
    }

    public void setRequestMethod(HttpMethod requestMethod) {
        this.requestMethod = requestMethod;
    }

    public void setStatus(HttpStatus status) {
        this.status = status;
    }

    public static HttpResponse newFixedLengthResponse(HttpStatus status, String mimeType, byte[] data) {
        return newFixedLengthResponse(status, mimeType, new ByteArrayInputStream(data), data.length);
    }

    public static HttpResponse newFixedLengthResponse(HttpStatus status, String mimeType, InputStream data, long totalBytes) {
        return new HttpResponse(status, mimeType, data, totalBytes);
    }

    public static HttpResponse newFixedLengthResponse(HttpStatus status, String mimeType, String txt) {
        ContentType contentType = new ContentType(mimeType);
        if (txt == null) {
            return newFixedLengthResponse(status, mimeType, new ByteArrayInputStream(new byte[0]), 0);
        } else {
            byte[] bytes;
            try {
                CharsetEncoder newEncoder = Charset.forName(contentType.getEncoding()).newEncoder();
                if (!newEncoder.canEncode(txt)) {
                    contentType = contentType.tryUTF8();
                }
                bytes = txt.getBytes(contentType.getEncoding());
            } catch (UnsupportedEncodingException e) {
                LOGGER.error("Encoding problem, responding nothing", e);
                bytes = new byte[0];
            }
            return newFixedLengthResponse(status, contentType.getContentTypeHeader(), new ByteArrayInputStream(bytes), bytes.length);
        }
    }

    public static HttpResponse newFixedLengthResponse(HttpStatus status, String msg) {
        return newFixedLengthResponse(status, MIME_JSON, msg);
    }

    public static HttpResponse newFixedLengthResponse(String msg) {
        return newFixedLengthResponse(HttpStatus.OK, MIME_JSON, msg);
    }

}

