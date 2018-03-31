package net.ionoff.center.shared.dto;

public class BaseDto implements IDto, Comparable<BaseDto> {

	private static final long serialVersionUID = 1L;	
	
	private long id;
	private String name;
	
	@Override
	public long getId() {
		return id;
	}
	
	@Override
	public void setId(long id) {
		this.id = id;
	}
	
	@Override
	public String getName() {
		return name;
	}
	@Override
	public void setName(String name) {
		this.name = name;
	}
	
	@Override
	public String formatNameID() {
		return formatNameID(name, id);
	}
	
	public static String formatNameID(BaseDto entity) {
		return BaseDto.formatNameID(entity.getName(), entity.getId());
	}
	
	public static String formatNameID(String name, Long id) {
		return name + " [#" + id + "]";
	}
	
	public static Long parseIdFromFormattedNameID(String formattedNameID) {
		String id = formattedNameID.split(" \\[#")[1].replaceAll("\\]", "");
		return Long.parseLong(id);
	}

	public static String parseNameFromFormattedNameID(String formattedNameID) {
		return formattedNameID.split(" \\[#")[0];
	}

	@Override
	public int compareTo(BaseDto entity) {
		if (name == null || entity.getName() == null) {
			return 0;
		}
		return formatNameID().compareTo(formatNameID(entity));
	}
	
	@Override
	public boolean isNew() {
		return id == DEFAULT_ID;
	}
	
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		
		builder.append("Id: ").append(getId())
				.append(", Name: " + getName());
		
		return builder.toString();
	}
}
