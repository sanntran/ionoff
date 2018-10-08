package net.ionoff.player.storage;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonReader;
import net.ionoff.player.config.AppConfig;
import net.ionoff.player.model.Schedule;
import net.ionoff.player.util.DatetimeUtil;
import org.apache.log4j.Logger;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.Writer;
import java.util.Date;

public class LocalStorage {
	
	private static final Logger LOGGER = Logger.getLogger(LocalStorage.class.getName());

	public static final LocalStorage INSTANCE = new LocalStorage();

	private final String file;
	private DataModel data;

    private String dir;

    private LocalStorage() {
        dir = AppConfig.INSTANCE.APP_DIR + File.separator + "storage";
		file =  dir + File.separator + "data.json";
	}

    public synchronized DataModel getData() {
        if (data == null) {
            read();
        }
        return data;
    }

    public String getDir() {
        return dir;
    }

    public void read() {
		File f = new File(file);
		if (!f.exists()) {
			data = newData();
			f.getParentFile().mkdirs();
		} else {
			try {
				Gson gson = new Gson();
				JsonReader reader = new JsonReader(new FileReader(f));
				data = gson.fromJson(reader, DataModel.class);
				data.setPassword(randomNumber() + "");
			} catch (Exception e) {
				data = newData();
				LOGGER.error("Error reading local data: " + e.getMessage(), e);
			}
		}
	}

	private DataModel newData() {
		data = new DataModel();
        data.setPassword(randomNumber() + "");
		data.setVolume(50);
		data.setSchedule(newSchedule());
		return data;
	}

	private int randomNumber() {
		return (int) (Math.random()*10000);
	}

	private static Schedule newSchedule() {
		Schedule schedule = new Schedule();
		schedule.setDateTime(DatetimeUtil.formatScheduleDateTime(new Date()));
		schedule.setAction(Schedule.NONE);
		return schedule;
	}

	public void write() {
		try (Writer writer = new FileWriter(file)) {
			Gson gson = new GsonBuilder().create();
			gson.toJson(data, writer);
		} catch (Exception e) {
			LOGGER.error("Error writing local data: " + e.getMessage(), e);
		}
	}

}
