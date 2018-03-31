package net.xapxinh.dataservice.config;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Logger;

public class DataServiceConfig {
	
	private static Logger logger = Logger.getLogger(DataServiceConfig.class.getName());

	private static DataServiceConfig INSTANCE = null;
	
	public final String FILESERVICE_URL;
	public final String FILESERVICE_FOLDER;
	public final String FILE_SERVICE_MUSIC;
	public final String DATA_FILES_MUSIC_REAL_PATH;
	public final String DATASERVICE_SEARCH_URL;
	public final boolean FILESERVICE_IS_LOSSLESS;
	public final List<String> YOUTUBE_AUDIO_FORMATS;
	
	public DataServiceConfig() {
		Properties prop = new Properties();
		try {
			InputStream stream = Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties");
			InputStreamReader isr = new InputStreamReader(stream, "UTF-8");
			prop.load(isr);
		}
		catch (IOException e) {
			logger.error(e.getMessage(), e);
		}
		FILESERVICE_URL = prop.getProperty("fileservice_url");
		FILESERVICE_FOLDER = prop.getProperty("fileservice_folder");
		FILE_SERVICE_MUSIC = prop.getProperty("fileservice_music");
		DATASERVICE_SEARCH_URL = prop.getProperty("dataservice_search_url");
		DATA_FILES_MUSIC_REAL_PATH = FILESERVICE_FOLDER + FILE_SERVICE_MUSIC;
		FILESERVICE_IS_LOSSLESS = Boolean.parseBoolean(prop.getProperty("fileservice_lossless"));
		YOUTUBE_AUDIO_FORMATS = new ArrayList<>();
		String formats = prop.getProperty("youtube_audio_formats");
		for (String f : formats.split(",")) {
			YOUTUBE_AUDIO_FORMATS.add(f);
		}
	}

	public static DataServiceConfig getInstance() {
		if (INSTANCE == null) {
			INSTANCE = new DataServiceConfig();
		}
		return INSTANCE;
	}

	public static boolean isWinPlatform() {
		String os = System.getProperty("os.name");
		if (os.contains("Win")) {
			return true;
		}
		return false;
	}

	public static boolean isLinuxPlatform() {
		String os = System.getProperty("os.name");
		if (os.contains("nux")) {
			return true;
		}
		return false;
	}
}
