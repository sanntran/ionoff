package net.ionoff.things.p8;

import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.MatteBorder;

class ControlPanel extends JPanel {
	
	private static final long serialVersionUID = 1L;

	private P8Tool p8Tool;
	
	private List<JLabel> statusLbls = new ArrayList<>(8);
	private List<JButton> onBtns = new ArrayList<>(8);
	private List<JButton> offBtns = new ArrayList<>(8);
	private List<JButton> onOffBtns = new ArrayList<>(8);
	
	
	private MatteBorder grayBorder = BorderFactory.createMatteBorder(0, 5, 0, 0, Color.LIGHT_GRAY);
	private MatteBorder blackBorder = BorderFactory.createMatteBorder(0, 5, 0, 0, Color.BLACK);
	private MatteBorder greenBorder = BorderFactory.createMatteBorder(0, 5, 0, 0, Color.GREEN);
	
	ControlPanel(P8Tool p8Tool) {
		
		this.p8Tool = p8Tool;
		
		setLayout(null);
		JPanel container = new JPanel();
		container.setBounds(10, 10, 340, 265);
		add(container);
		
		container.setLayout(new GridLayout(8, 4, 8, 8));
		
		onBtns = new ArrayList<>(8);
		offBtns = new ArrayList<>(8);
		onOffBtns = new ArrayList<>(8);
		
		for (int i = 0; i< 8; i++) {
			
			final int relayIndex = i + 1;
			
			JLabel relayName = new JLabel("  Relay " + (i + 1));
			relayName.setSize(100, 25);
			relayName.setBorder(grayBorder);
			container.add(relayName);
			statusLbls.add(relayName);
			
			JButton onBtn = new JButton("On");	
			onBtn.setSize(100, 26);
			container.add(onBtn);
			onBtns.add(onBtn);
			
			JButton offBtn = new JButton("Off");	
			offBtn.setSize(100, 26);
			container.add(offBtn);
			offBtns.add(offBtn);
			
			JButton onOffBtn = new JButton("On-Off");	
			onOffBtn.setSize(100, 26);
			container.add(onOffBtn);
			onOffBtns.add(onOffBtn);
			
			onBtn.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					setControlStatus(relayIndex, 1);
				}
			});
			
			offBtn.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					setControlStatus(relayIndex, 0);
				}
			});
			
			onOffBtn.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					setControlStatus(relayIndex, 2);
				}
			});
		}
	}

	void setControlStatus(int relayIndex, int controlCode) {
		if (!p8Tool.validateIp()) {
			p8Tool.showMessage("IP is invalid", Color.RED);
			return;
		}
		if (!p8Tool.validatePw()) {
			p8Tool.showMessage("PW is invalid", Color.RED);
			return;
		}
		try {
			P8Status status = TcpServer.getInstance().sendReqSetStatus(p8Tool.getPwTextField().getText(), relayIndex, controlCode);
			setStatus(status);
		} catch (Exception e) {
			e.printStackTrace();
			p8Tool.showMessage(e.getMessage(), Color.RED);
		}
	}

	void setStatus(P8Status status) {
		if (status == null) {
			for (int i = 0; i < 8; i++) {
				statusLbls.get(i).setBorder(grayBorder);
			}
			return;
		}
		for (int i = 0; i < 8; i++) {
			if (status.getOutputStates().get(i) == null) {
				statusLbls.get(i).setBorder(grayBorder);
			}
			else if (status.getOutputStates().get(i) == true) {
				statusLbls.get(i).setBorder(greenBorder);
			}
			else if (status.getOutputStates().get(i) == false) {
				statusLbls.get(i).setBorder(blackBorder);
			}
		}
	}
	
	void setConnected() {
		for (int i = 0; i < 8; i++) {
			onBtns.get(i).setEnabled(true);
			offBtns.get(i).setEnabled(true);
			onOffBtns.get(i).setEnabled(true);
		}
	}
	void setDisConnected() {
		setStatus(null);
		for (int i = 0; i < 8; i++) {
			onBtns.get(i).setEnabled(false);
			offBtns.get(i).setEnabled(false);
			onOffBtns.get(i).setEnabled(false);
		}
	}
}
