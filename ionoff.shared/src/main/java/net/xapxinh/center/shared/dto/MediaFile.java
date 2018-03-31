package net.xapxinh.center.shared.dto;

public class MediaFile extends SerializableDto {

	private static final long serialVersionUID = 1L;
	
	public enum TYPE {
		file, dir;
	}
	
	private String type;
	private String name;
	private String path;
	private String access_time;
	private String uid;
	private String creation_time;
	private String gid;
	private String modification_time;
	private String mode;
	private String size;

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getName() {
		return name;
	}
	
	public String getDisplayName() {
		if (isAlbum()) {
			return name.substring(0, name.lastIndexOf('#'));
		}
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getAccess_time() {
		return access_time;
	}

	public void setAccess_time(String access_time) {
		this.access_time = access_time;
	}

	public String getUid() {
		return uid;
	}

	public void setUid(String uid) {
		this.uid = uid;
	}

	public String getCreation_time() {
		return creation_time;
	}

	public void setCreation_time(String creation_time) {
		this.creation_time = creation_time;
	}

	public String getGid() {
		return gid;
	}

	public void setGid(String gid) {
		this.gid = gid;
	}

	public String getModification_time() {
		return modification_time;
	}

	public void setModification_time(String modification_time) {
		this.modification_time = modification_time;
	}

	public String getMode() {
		return mode;
	}

	public void setMode(String mode) {
		this.mode = mode;
	}

	public String getSize() {
		return size;
	}

	public void setSize(String size) {
		this.size = size;
	}
	
	public boolean isAlbum() {
		return path != null && path.contains("#") && path.endsWith(".album");
	}
	
	public String getParent() {
		return  getParent(path);
	}
	
	public static String getParent(String filePath) {
		if (filePath == null || filePath.isEmpty()) {
			return null;
		}
		if (filePath.contains("/")) {
			return filePath.substring(0, filePath.lastIndexOf('/'));
		}
		else if (filePath.contains("\\")) {
			return filePath.substring(0, filePath.lastIndexOf("\\"));
		}
		return  null;
	}
}
