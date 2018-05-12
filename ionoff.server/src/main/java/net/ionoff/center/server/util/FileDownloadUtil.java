package net.ionoff.center.server.util;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.net.URLConnection;

public class FileDownloadUtil {
	
	public static String downloadFile(String fromUrl, String localFileName) throws IOException {
		File localFile = new File(localFileName);
		if (localFile.exists()) {
			localFile.delete();
		}
		localFile.createNewFile();
		URL url = new URL(fromUrl);
		OutputStream out = new BufferedOutputStream(new FileOutputStream(localFileName));
		URLConnection conn = url.openConnection();
		InputStream in = conn.getInputStream();
		byte[] buffer = new byte[1024];
		int numRead;
		while ((numRead = in.read(buffer)) != -1) {
			out.write(buffer, 0, numRead);
		}
		if (in != null) {
			in.close();
		}
		if (out != null) {
			out.close();
		}
		return localFileName;
	}
}
