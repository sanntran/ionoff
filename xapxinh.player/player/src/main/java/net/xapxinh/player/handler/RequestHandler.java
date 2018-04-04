package net.xapxinh.player.handler;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.servlet.http.HttpServletRequest;

import net.xapxinh.player.Application;
import net.xapxinh.player.EmbeddedMediaPlayerPanel;
import net.xapxinh.player.config.UserConfig;
import net.xapxinh.player.connection.TcpRequest;
import net.xapxinh.player.model.MediaFile;
import net.xapxinh.player.model.PlayList;
import net.xapxinh.player.model.Schedule;
import net.xapxinh.player.model.Status;
import net.xapxinh.player.server.exception.DateTimeFormatException;
import net.xapxinh.player.server.exception.UnknownCommandException;
import net.xapxinh.player.server.exception.UnknownContextException;
import uk.co.caprica.vlcj.filter.MediaFileFilter;

public class RequestHandler {
	
	protected static final String COMMAND = "command";

	private static final String CONTEXT_STATUS = "/status";
	private static final String CONTEXT_PLAYLIST = "/playlist";
	private static final String CONTEXT_BROWSE = "/browse";
	private static final String CONTEXT_SCHEDULE = "/schedule";
	
	private final Map<String, String> params;
	private final EmbeddedMediaPlayerPanel mediaPlayerPanel;
	
	RequestHandler(HttpServletRequest httpRequest) {
		params = getParammeters(httpRequest);
		mediaPlayerPanel = Application.application().mediaPlayerPanel();
	}
	
	public RequestHandler(TcpRequest tcpRequest) {
		params = tcpRequest.getParameters();
		mediaPlayerPanel = Application.application().mediaPlayerPanel();
	}
	
	protected Map<String, String> getParammeters(final HttpServletRequest httpRequest) {
		Map<String, String> params = new HashMap<String, String>();
		
		String context = httpRequest.getContextPath() + httpRequest.getPathInfo();
		params.put(RequestContext.CONTEXT, context);
		
		for (Entry<String, String[]> entry : httpRequest.getParameterMap().entrySet()) {
			String param = entry.getKey();
			params.put(param, httpRequest.getParameter(param));
		}
		return params;
	}
	
	protected Map<String, String> getParammeters(final TcpRequest tcpRequest) {
		return tcpRequest.getParameters();
	}

	public String handleRequest() throws Exception {
		return handleRequest(params);
	}
	
	private String handleRequest(Map<String, String> params) throws Exception {
		final String context = params.get(RequestContext.CONTEXT);
		params.remove(RequestContext.CONTEXT);
		
		if (context.equals(RequestContext.REQUESTS + CONTEXT_STATUS)) {
			return handleStatusRequest(params);
		}
		if (context.equals(RequestContext.REQUESTS + CONTEXT_PLAYLIST)) {
			return handlePlaylistRequest(params);
		}
		if (context.equals(RequestContext.REQUESTS + CONTEXT_BROWSE)) {
			return handleBrowseRequest(params);
		}
		if (context.equals(RequestContext.REQUESTS + CONTEXT_SCHEDULE)) {
			return handleScheduleRequest(params);
		}
		throw new UnknownContextException(context);
	}

	private String handleScheduleRequest(Map<String, String> params) throws UnknownCommandException, DateTimeFormatException {
		Schedule schedule = new ScheduleRequestHandler().handleRequest(params);
		
		return new PlayerResponse("schedule", schedule).toJSONString();
	}

	private String handleStatusRequest(final Map<String, String> params) {
		final Status status = getStatus(params);
		return new PlayerResponse("status", status).toJSONString();
	}
	
	private Status getStatus(Map<String, String> params) {
		return new StatusRequestHandler(mediaPlayerPanel).handleRequest(params);
	}

	private String handlePlaylistRequest(final Map<String, String> params) throws Exception {
		PlayList playlist = new PlaylistRequestHandler(mediaPlayerPanel).handleRequest(params);
		return new PlayerResponse("playlist", playlist).toJSONString();
	}

	static String handleBrowseRequest(final Map<String, String> params) throws Exception {
		List<MediaFile> files = getBrowse(params);
		return new PlayerResponse("files", files).toJSONString();
	}

	private static List<MediaFile> getBrowse(Map<String, String> params) {
		String dir = params.get("dir");
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
