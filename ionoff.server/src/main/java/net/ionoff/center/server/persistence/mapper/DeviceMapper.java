package net.ionoff.center.server.persistence.mapper;

import net.ionoff.center.server.entity.RelayLoad;
import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.EntityUtil;
import net.ionoff.center.server.entity.MediaPlayer;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.mediaplayer.service.IMediaPlayerService;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.dto.RelayLoadDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MediaPlayerDto;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.player.StatusDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class DeviceMapper {
	
	@Autowired
	private RelayMapper relayMappper;
	
	public Device createDevice(DeviceDto deviceDto, Zone zone) {
		final Device device = createNewDevice(deviceDto);
		updateDevice(device, deviceDto);
		device.setZone(zone);
		device.setProject(zone.getProject());
		return device;
	}

	public Device updateDevice(Device device, DeviceDto deviceDto) {
		if (device instanceof MediaPlayer) {
			final MediaPlayerDto playerDto = (MediaPlayerDto)deviceDto;
			MediaPlayer player = (MediaPlayer) device;
			if (isMacChanged(player.getMac(), playerDto.getMac())) {
				player.setStatus(null);
				player.setTime(null);
			}
			player.setMac(playerDto.getMac());
			player.setModel(playerDto.getModel());
		}
		device.setName(deviceDto.getName());
		device.setOrder(deviceDto.getOrder());
		return device;
	}

	private boolean isMacChanged(String mac, String macDto) {
		if (mac == null && macDto != null) {
			return true;
		}
		if (mac != null && !mac.equals(macDto)) {
			return true;
		}
		return false;
	}

	private Device createNewDevice(DeviceDto deviceDto) {
		if (deviceDto instanceof MediaPlayerDto) {
			final MediaPlayer player = new MediaPlayer();
			return player;
		}
		return new RelayLoad();
	}
	
	public List<DeviceDto> toDeviceDtoList(List<? extends Device> devices , IMediaPlayerService playerService) {
		final List<DeviceDto> deviceDtos = new ArrayList<DeviceDto>();
		for (final Device device : devices) {
			deviceDtos.add(createDeviceDto(device, playerService));
		}
		return deviceDtos;
	}
	
	public DeviceDto createDeviceDto(Device device, IMediaPlayerService playerService) {
		final DeviceDto deviceDto = newDeviceDto(EntityUtil.castUnproxy(device, Device.class));
		deviceDto.setStatus(getStatusDto(device, playerService));
		deviceDto.setId(device.getId());
		deviceDto.setName(device.getName());
		deviceDto.setOrder(device.getOrder());
		deviceDto.setZoneId(device.getZone().getId());
		deviceDto.setZoneName(device.getZone().getName());
		deviceDto.setAreaName(device.getZone().getArea().getName());
		deviceDto.setProjectId(device.getProject().getId());
		return deviceDto;
	}

	public List<net.ionoff.center.shared.dto.StatusDto> toStatusDto(List<Device> devices, IMediaPlayerService playerService) {
		List<net.ionoff.center.shared.dto.StatusDto> statusDtos = new ArrayList<>();
		for (Device device : devices) {
			statusDtos.add(getStatusDto(device, playerService));
		}
		return statusDtos;
	}
	
	public net.ionoff.center.shared.dto.StatusDto getStatusDto(Device device, IMediaPlayerService playerService) {
		net.ionoff.center.shared.dto.StatusDto statusDto = new net.ionoff.center.shared.dto.StatusDto();
		statusDto.setId(device.getId());
		statusDto.setValue(device.getStatus());
		if (device.getTime() != null) {
			statusDto.setTime(DateTimeUtil.ddMMHHmmFormatter.format(device.getTime()));
		}
		if (device.instanceOf(RelayLoad.class)) {
			for (Relay relay : device.getRelayList()) {
				net.ionoff.center.shared.dto.StatusDto child = new net.ionoff.center.shared.dto.StatusDto();
				child.setId(relay.getId());
				child.setValue(relay.getStatus());
				if (relay.getTime() != null) {
					child.setTime(DateTimeUtil.ddMMHHmmFormatter.format(relay.getTime()));
				}
				statusDto.getChildren().add(child);
			}
		}
		else if (device.instanceOf(MediaPlayer.class) && playerService != null) {
			try {
				MediaPlayer player = EntityUtil.castUnproxy(device, MediaPlayer.class);
				StatusDto status = playerService.requesStatus(net.ionoff.center.server.mediaplayer.model.MediaPlayer.fromPlayer(player), null);
				statusDto.setState(status.getState());
				statusDto.setTrack(status.getTitle());
				if (status.getPosition() > 0) {
					statusDto.setPosition(Math.round(status.getPosition() * 100));
				}
			} catch (Exception e) {
				//
			}
		}
		return statusDto;
	}

	public DeviceDto newDeviceDto(Device device) {
		if (device instanceof MediaPlayer) {
			return createMediaPlayerDto(device);
		}
		return createRelayLoadDto(device);
	}

	private RelayLoadDto createRelayLoadDto(Device device) {
		final RelayLoadDto applianceDto = new RelayLoadDto();
		final List<RelayDto> relayDtos = new ArrayList<RelayDto>();
		for (final Relay relay : device.getRelayList()) {
			relayDtos.add(relayMappper.createRelayDto(relay));
		}
		applianceDto.setRelays(relayDtos);
		return applianceDto;
	}
	
	private static DeviceDto createMediaPlayerDto(Device device) {
		final MediaPlayerDto playerDto = new MediaPlayerDto();
		final MediaPlayer player = (MediaPlayer) device;
		playerDto.setMac(player.getMac());
		playerDto.setIp(player.getIp());
		playerDto.setModel(player.getModel());
		return playerDto;
	}

	public DeviceDto createDeviceDto(Device device) {
		return createDeviceDto(device, null);
	}
}

