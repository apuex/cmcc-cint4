package com.github.apuex.cmcc.cint4.utility;


import java.util.Map;

import io.netty.util.ReferenceCountUtil;
import org.slf4j.LoggerFactory;

import com.github.apuex.cmcc.cint4.HeartBeatAck;
import com.github.apuex.cmcc.cint4.Login;
import com.github.apuex.cmcc.cint4.Logout;
import com.github.apuex.cmcc.cint4.Message;
import com.github.apuex.cmcc.cint4.ModifyPA;

import ch.qos.logback.classic.Logger;
import io.netty.channel.ChannelHandlerContext;

public class ModifyPasswdHandler extends io.netty.channel.ChannelInboundHandlerAdapter {
	private static final Logger logger = (Logger) LoggerFactory.getLogger(ModifyPasswdHandler.class);
	private Map<String, String> params;

	public ModifyPasswdHandler(Map<String, String> params) {
		this.params = params;
	}
	@Override
	public void channelActive(ChannelHandlerContext ctx) throws Exception {
		logger.info(String.format("[%s] SYN : connection established.", ctx.channel().remoteAddress()));
		SerialNo.initSerialNo(ctx.channel());
		send(ctx, new Login(SerialNo.nextSerialNo(ctx.channel()), params.get("server-user"), params.get("server-passwd")));
		ctx.fireChannelActive();
	}

	@Override
	public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
		logger.info(String.format("[%s] RCV : %s", ctx.channel().remoteAddress(), msg));
		if(msg instanceof Message) {
			Message message = (Message) msg;
			switch(message.PKType) {
			case LOGIN:
				break;
			case LOGIN_ACK:
				send(ctx, new ModifyPA(SerialNo.nextSerialNo(ctx.channel())
						, params.get("server-user")
						, params.get("server-passwd")
						, params.get("server-new-passwd") == null ? params.get("server-passwd") : params.get("server-new-passwd")));
				break;
			case LOGOUT:
				break;
			case LOGOUT_ACK:
				ctx.close();
				break;
			case SET_DYN_ACCESS_MODE:
				break;
			case DYN_ACCESS_MODE_ACK:
				break;
			case SET_ALARM_MODE:
				break;
			case ALARM_MODE_ACK:
				break;
			case SEND_ALARM:
				break;
			case SEND_ALARM_ACK:
				break;
			case SET_POINT:
				break;
			case SET_POINT_ACK:
				break;
			case MODIFY_PA:
				break;
			case MODIFY_PA_ACK:
				send(ctx, new Logout(SerialNo.nextSerialNo(ctx.channel())));
				break;
			case HEART_BEAT:
				send(ctx, new HeartBeatAck(message.SerialNo));
				break;
			case HEART_BEAT_ACK:
				break;
			case TIME_CHECK:
				break;
			case TIME_CHECK_ACK:
				break;
			default: // Unsupported PKType
				ctx.close();
				break;
			}
		}
		ReferenceCountUtil.release(msg);
	}

	@Override
	public void channelInactive(ChannelHandlerContext ctx) throws Exception {
		logger.info(String.format("[%s] RST : connection closed.", ctx.channel().remoteAddress()));
		ctx.fireChannelInactive();
	}

	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
		logger.info(String.format("[%s] ERR : %s", ctx.channel().remoteAddress(), cause));
		ctx.fireExceptionCaught(cause);
	}

	private void send(ChannelHandlerContext ctx, Message out) {
		ctx.writeAndFlush(out);
	}
}
