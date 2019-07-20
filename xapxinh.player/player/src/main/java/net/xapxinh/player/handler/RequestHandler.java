package net.xapxinh.player.handler;

import net.xapxinh.player.Application;
import net.xapxinh.player.EmbeddedMediaPlayerPanel;
import net.xapxinh.player.config.UserConfig;
import net.xapxinh.player.connection.MqttRequestMessage;
import net.xapxinh.player.model.MediaFile;
import net.xapxinh.player.model.PlayList;
import net.xapxinh.player.model.Schedule;
import net.xapxinh.player.model.Status;
import net.xapxinh.player.server.exception.DateTimeFormatException;
import net.xapxinh.player.server.exception.UnknownCommandException;
import net.xapxinh.player.server.exception.UnknownContextException;
import uk.co.caprica.vlcj.filter.MediaFileFilter;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class RequestHandler {

	private static final String CONTEXT_STATUS = "/requests/status";
	private static final String CONTEXT_PLAYLIST = "/requests/playlist";
	private static final String CONTEXT_BROWSE = "/requests/browse";
	private static final String CONTEXT_SCHEDULE = "/requests/scheduler";
	
	private final EmbeddedMediaPlayerPanel mediaPlayerPanel;

	public RequestHandler() {
		mediaPlayerPanel = Application.application().mediaPlayerPanel();
	}


	public PlayerResponse handleRequest(MqttRequestMessage request) throws Exception {
		final String context = request.getContext();
		if (CONTEXT_STATUS.equals(context)) {
			return handleStatusRequest(request);
		}
		if (CONTEXT_PLAYLIST.equals(context)) {
			return handlePlaylistRequest(request);
		}
		if (CONTEXT_BROWSE.equals(context)) {
			return handleBrowseRequest(request);
		}
		if (CONTEXT_SCHEDULE.equals(context)) {
			return handleScheduleRequest(request);
		}
		throw new UnknownContextException(context);
	}

	public PlayerResponse handleStatusRequest() {
		final Status status = new StatusRequestHandler(mediaPlayerPanel).handleRequest(null, null);
		return new PlayerResponse("status", status);
	}

	private PlayerResponse handleScheduleRequest(MqttRequestMessage request) throws UnknownCommandException, DateTimeFormatException {
		Schedule schedule = new ScheduleRequestHandler().handleRequest(request);
		return new PlayerResponse("schedule", schedule);
	}

	private PlayerResponse handleStatusRequest(MqttRequestMessage request) {
		final Status status = getStatus(request);
		return new PlayerResponse("status", status);
	}
	
	private Status getStatus(MqttRequestMessage request) {
		return new StatusRequestHandler(mediaPlayerPanel).handleRequest(request);
	}

	private PlayerResponse handlePlaylistRequest(final MqttRequestMessage request) throws Exception {
		PlayList playlist = new PlaylistRequestHandler(mediaPlayerPanel).handleRequest(request);
		return new PlayerResponse("playlist", playlist);
	}

	static PlayerResponse handleBrowseRequest(final MqttRequestMessage request) throws Exception {
		List<MediaFile> files = getBrowse(request);
		return new PlayerResponse("files", files);
	}

	private static List<MediaFile> getBrowse(MqttRequestMessage request) {
		String dir = request.getAsString("dir");
		if (dir == null || dir.isEmpty() || !dir.startsWith(UserConfig.getInstance().ROOT_BROWSE_DIR) 
				|| dir.equalsIgnoreCase(UserConfig.getInstance().ROOT_BROWSE_DIR + "/..")
				|| dir.equalsIgnoreCase(UserConfig.getInstance().ROOT_BROWSE_DIR + "\\..")) {
			dir = UserConfig.getInstance().ROOT_BROWSE_DIR;
		}
		return getMediaFiles(dir);
	}

	private static List<MediaFile> getMediaFiles(String dir) {
		
		List<MediaFile> mediaFiles = new ArrayList<>();
		File file = new File(dir);
		
		if (!file.exists() || file.isFile()) {
			return mediaFiles;
		}
		try {
			String canonicalDir = file.getCanonicalPath();
			file = new File(canonicalDir);
			
			MediaFile parent = new MediaFile();
			parent.setName("..");
			parent.setPath(canonicalDir + File.separator + "..");
			parent.setType("dir");
			mediaFiles.add(parent);
			
			for (File f : file.listFiles()) {
					MediaFile mFile = new MediaFile();
					mFile.setName(f.getName());
					mFile.setPath(f.getAbsolutePath());
					if (f.isDirectory()) {
						mFile.setType("dir");
					}
					else {
						mFile.setType("file");
					}
					if (mFile.isAlbum() || (MediaFileFilter.INSTANCE.accept(f) && f.isFile())) {
						mediaFiles.add(mFile);
					}
					else if (f.isDirectory()) {
						mediaFiles.add(mFile);
					}
			}
			return mediaFiles;
		} catch (IOException e) {
			return mediaFiles;
		}
	}
}
