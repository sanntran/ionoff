package net.xapxinh.center.shared.dto;




public class SingleSerialDto implements RpcResponseDto {

	private static final long serialVersionUID = 1L;

	private SerializableDto serializableDto;

	public SerializableDto getSerializableDto() {
		return serializableDto;
	}

	public void setSerializableDto(SerializableDto serializableDto) {
		this.serializableDto = serializableDto;
	}
}
