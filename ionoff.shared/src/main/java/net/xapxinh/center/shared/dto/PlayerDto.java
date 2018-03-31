package net.xapxinh.center.shared.dto;

public class PlayerDto extends SerializableDto {

	private static final long serialVersionUID = 1L;
	public static final String P = "P";

	public enum STATUS {
		DISCONNECTED(0), CONNECTED(1);
		
		private int value;
		
		private STATUS(int value) {
			this.value = value;
		}
		
		public int getValue() {
			return this.value;
		}
	}
	
	private long id;
	
	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}
}
